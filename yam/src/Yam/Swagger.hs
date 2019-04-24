{-# LANGUAGE NoPolyKinds #-}
module Yam.Swagger(
    SwaggerConfig(..)
  , serveWithContextAndSwagger
  , baseInfo
  , SwaggerTag
  ) where

import           Control.Lens       hiding (Context)
import           Data.Reflection
import           Data.Swagger
import           Data.Version       (showVersion)
import           GHC.TypeLits
import           Salak
import           Servant
import           Servant.Client
import           Servant.Swagger
import           Servant.Swagger.UI
import           Yam.Prelude

-- | Swagger Configuration
data SwaggerConfig = SwaggerConfig
  { urlDir    :: String -- ^ Url path for swagger.
  , urlSchema :: String -- ^ Api schema path for swagger.
  , enabled   :: Bool   -- ^ If enable swagger.
  } deriving (Eq, Show)

instance FromProp SwaggerConfig where
  fromProp = SwaggerConfig
    <$> "dir"     .?= "swagger-ui"
    <*> "schema"  .?= "swagger-ui.json"
    <*> "enabled" .?= True

-- | Serve with swagger.
serveWithContextAndSwagger
  :: forall api context. (HasSwagger api, HasServer api context)
  => SwaggerConfig -- ^ Swagger configuration.
  -> (Swagger -> Swagger) -- ^ Swagger modification.
  -> Proxy api -- ^ Application API Proxy.
  -> Context context -- ^ Application context.
  -> ServerT api Handler -- ^ Application API Server
  -> Application
serveWithContextAndSwagger SwaggerConfig{..} g5 proxy cxt api =
  if enabled
    then reifySymbol urlDir $ \pd -> reifySymbol urlSchema $ \ps ->
         serveWithContext (go proxy pd ps) cxt (swaggerSchemaUIServer (g5 $ toSwagger proxy) :<|> api)
    else serveWithContext proxy cxt api
  where
    go :: forall dir schema. Proxy api -> Proxy dir -> Proxy schema -> Proxy (SwaggerSchemaUI dir schema :<|> api)
    go _ _ _ = Proxy

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

