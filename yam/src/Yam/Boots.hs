{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}
module Yam.Boots where

import           Boots
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default
import qualified Data.HashMap.Strict                 as HM
import           Data.Proxy
import           Data.Text                           (Text, pack)
import           GHC.Generics
import           Lens.Micro
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Control.Exception
    ( Exception (..)
    , SomeException
    )
import           Data.Maybe
import           Data.Reflection
import           Data.Swagger                        hiding (name, port)
import           Data.Version                        (Version, showVersion)
import           GHC.TypeLits
import           Salak
import           Servant
import           Servant.Client
import           Servant.Server.Internal.ServerError (responseServerError)
import           Servant.Swagger
import           Servant.Swagger.UI

-- | Application Configuration.
data AppConfig = AppConfig
  { name          :: Text   -- ^ Application name.
  , hostname      :: String -- ^ Applicatoin hostname, used in swagger.
  , port          :: Int    -- ^ Application http port.
  , slowlorisSize :: Int    -- ^ Slowloris size in Bytes, show in 'Settings'
  } deriving (Eq, Show)

instance Default AppConfig where
  def = AppConfig "application" "localhost" 8888 2048

instance Monad m => FromProp m AppConfig where
  fromProp = AppConfig
    <$> "name"           .?: name
    <*> "host"           .?: hostname
    <*> "port"           .?: port
    <*> "slowloris-size" .?: slowlorisSize

data HealthStatus = UP | DOWN deriving (Eq, Show, Generic, ToJSON)

data Health = Health
  { status  :: HealthStatus
  , message :: Maybe String
  , details :: HM.HashMap Text Health
  } deriving (Eq, Show, Generic, ToJSON)

emptyHealth :: IO Health
emptyHealth = return (Health UP Nothing HM.empty)

data WebContext = WebContext
  { simple      :: Simple
  , app         :: AppConfig
  , middleware  :: Middleware
  , checkHealth :: MVar (IO Health)
  , serveWeb    :: forall api. HasServer api '[WebContext] => Proxy api -> Context '[WebContext] -> Server api -> Application
  }

webPlugin :: (MonadCatch m, MonadIO m) => Plugin Simple m WebContext
webPlugin = do
  i           <- ask
  app         <- require "application"
  logInfo $ "Start Service [" <> name app <> "] ..."
  checkHealth <- liftIO $ newMVar emptyHealth
  return $ WebContext i app id checkHealth serveWithContext

type AppW = App WebContext

runWeb :: (HasSwagger api, HasServer api '[WebContext])
  => Version
  -> Proxy api
  -> ServerT api AppW
  -> IO ()
runWeb v proxy server = boot go
  where
    go = do
      i <- pluginSimple "application"
      c <- promote i $ webPlugin
      a <- promote c
        $ actuatorPMW
        $ swaggerPMW proxy v id
        $ servePlugin proxy (c :. EmptyContext) server
      promote i $ logInfo $ "Servant started on port(s): " <> showText (port $ app c)
      return $ serveWarp (app c) a

-- | default http server by warp.
serveWarp :: AppConfig -> Application -> IO ()
serveWarp AppConfig{..} = runSettings
  $ defaultSettings
  & setPort port
  & setOnException (\_ _ -> return ())
  & setOnExceptionResponse whenException
  & setSlowlorisSize slowlorisSize

-- | Convert exception to 'Response'
whenException :: SomeException -> Network.Wai.Response
whenException e = responseServerError $ fromMaybe err400 { errBody = encode $ WebErrResult $ showText e} (fromException e :: Maybe ServerError)
newtype WebErrResult = WebErrResult
  { message :: Text
  }

instance ToJSON WebErrResult where
  toJSON WebErrResult{..} = object [ "message" .= message ]
-- | Show text.
{-# INLINE showText #-}
showText :: Show a => a -> Text
showText = pack . show

class HasWeb cxt where
  askWeb :: Lens' cxt WebContext

instance HasWeb WebContext where
  askWeb = id

instance HasSimple WebContext where
  askSimple = lens simple (\x y -> x { simple = y})

instance HasSalak WebContext where
  askSourcePack = askSimple . askSourcePack

instance HasLogger WebContext where
  askLogger = askSimple . askLogger

-- ** Serve
servePlugin
  :: HasServer api '[WebContext]
  => Proxy api
  -> Context '[WebContext]
  -> ServerT api AppW
  -> Plugin WebContext m Application
servePlugin p c s = do
  WebContext{..} <- ask
  return $ serveWeb p c (hoistServerWithContext p (Proxy @('[WebContext])) (runNT c) s)


type PluginMiddleware m  = Plugin WebContext m Application -> Plugin WebContext m Application

serveWhen :: forall api x m. (x ~ '[WebContext], HasServer api x) => Bool -> Proxy api -> ServerT api AppW -> PluginMiddleware m
serveWhen en p s pimu
  | en = withPlugin (\wc -> wc { serveWeb = \a b c -> serveWeb wc (proxyAdd a p) b (c :<|> hoistServerWithContext p (Proxy @x) (runNT b) s) }) pimu
  | otherwise = pimu

serveWhen' :: forall api m x. (x ~ '[WebContext], HasServer api x) => Bool -> Proxy api -> ServerT api Handler -> PluginMiddleware m
serveWhen' en p s pimu
  | en = withPlugin (\wc -> wc { serveWeb = \a b c -> serveWeb wc (proxyAdd a p) b (c :<|> s) }) pimu
  | otherwise = pimu

runNT :: Context '[cxt] -> AppT cxt IO a -> Handler a
runNT (c :. EmptyContext) ma = liftIO $ runAppT c ma

proxyAdd :: Proxy a -> Proxy b -> Proxy (a :<|> b)
proxyAdd _ _ = Proxy

-- ** Swagger
-- | Swagger Configuration
data SwaggerConfig = SwaggerConfig
  { urlDir    :: String -- ^ Url path for swagger.
  , urlSchema :: String -- ^ Api schema path for swagger.
  , enabled   :: Bool   -- ^ If enable swagger.
  } deriving (Eq, Show)

instance Monad m => FromProp m SwaggerConfig where
  fromProp = SwaggerConfig
    <$> "dir"     .?= "swagger-ui"
    <*> "schema"  .?= "swagger-ui.json"
    <*> "enabled" .?= True

-- | Swagger modification
baseInfo
  :: String  -- ^ Hostname
  -> Text    -- ^ Server Name
  -> Version -- ^ Server version
  -> Int     -- ^ Port
  -> Swagger -- ^ Old swagger
  -> Swagger
baseInfo hostName n v p s = s
  & info . title   .~ n
  & info . version .~ pack (showVersion v)
  & host ?~ Host hostName (Just $ fromIntegral p)

data SwaggerTag (name :: Symbol) (desp :: Symbol)

instance HasServer api ctx
  => HasServer (SwaggerTag name desp :> api) ctx where
  type ServerT (SwaggerTag name desp :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance HasClient m api
  => HasClient m (SwaggerTag name desp :> api) where
  type  Client m (SwaggerTag name desp :> api) = Client m api
  clientWithRoute _ _ = clientWithRoute (Proxy @m) (Proxy @api)
  hoistClientMonad pm _ = hoistClientMonad pm (Proxy @api)

instance (HasSwagger api, KnownSymbol name, KnownSymbol desp)
  => HasSwagger (SwaggerTag name desp :> api) where
  toSwagger _ = toSwagger (Proxy @api) & applyTags [tag]
    where
      tag = Tag (go (Proxy @name)) (g2 $ go (Proxy @desp)) Nothing
      go :: forall a. KnownSymbol a => Proxy a -> Text
      go  = pack . symbolVal
      g2 "" = Nothing
      g2 a  = Just a

swaggerPMW :: (MonadThrow m, HasSwagger api, MonadIO m) => Proxy api -> Version -> (Swagger -> Swagger) -> PluginMiddleware m
swaggerPMW proxy v ff pimu = do
  SwaggerConfig{..} <- require "swagger"
  WebContext{..}    <- ask
  let AppConfig{..} = app
  when enabled $
    logInfo  $ "Swagger enabled: http://localhost:" <> pack (show port) <> "/" <> pack urlDir
  reifySymbol urlDir
    $ \pd -> reifySymbol urlSchema
    $ \ps -> serveWhen' enabled (gop pd ps) (swaggerSchemaUIServer $ ff $ baseInfo hostname name v port $ toSwagger proxy) pimu
  where
    gop :: forall a b. Proxy a -> Proxy b -> Proxy (SwaggerSchemaUI a b)
    gop _ _ = Proxy

-- **
data ActuatorConfig = ActuatorConfig
  { enabled :: Bool
  , refresh :: Bool
  , health  :: Bool
  }

instance Monad m => FromProp m ActuatorConfig where
  fromProp = ActuatorConfig
    <$> "enabled"         .?= False
    <*> "refresh.enabled" .?= True
    <*> "health.enabled"  .?= True

actuatorPMW :: PluginMiddleware m
actuatorPMW pimu = pimu
