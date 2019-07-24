module Yam.Middleware.Swagger(
    SwaggerConfig(..)
  , swaggerMiddleware
  , baseInfo
  , SwaggerTag
  ) where

import           Control.Lens         hiding (Context)
import           Control.Monad.Reader
import           Data.Proxy
import           Data.Reflection
import           Data.Swagger         hiding (name, port)
import           Data.Version         (showVersion)
import           GHC.TypeLits
import           Salak
import           Servant
import           Servant.Client
import           Servant.Swagger
import           Servant.Swagger.UI
import           Yam.Internal
import           Yam.Middleware

-- | Swagger Configuration
data SwaggerConfig = SwaggerConfig
  { urlDir    :: String -- ^ Url path for swagger.
  , urlSchema :: String -- ^ Api schema path for swagger.
  , enabled   :: Bool   -- ^ If enable swagger.
  } deriving (Eq, Show)

instance MonadCatch m => FromProp m SwaggerConfig where
  fromProp = SwaggerConfig
    <$> "dir"     .?= "swagger-ui"
    <*> "schema"  .?= "swagger-ui.json"
    <*> "enabled" .?= True

swaggerMiddleware :: (HasSwagger api, HasBase cxt, MonadIO m) => Proxy api -> Version -> (Swagger -> Swagger) -> AppMiddleware m amtdcxt cxt ()
swaggerMiddleware proxy v ff = do
  SwaggerConfig{..} <- require "swagger"
  AppConfig{..}     <- askCxt
  when enabled $
      logInfo  $ "Swagger enabled: http://localhost:" <> pack (show port) <> "/" <> pack urlDir
  reifySymbol urlDir
    $ \pd -> reifySymbol urlSchema
    $ \ps -> modifyServer' enabled (gop pd ps) (swaggerSchemaUIServer $ ff $ baseInfo hostname name v port $ toSwagger proxy)
  where
    gop :: forall a b. Proxy a -> Proxy b -> Proxy (SwaggerSchemaUI a b)
    gop _ _ = Proxy

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
