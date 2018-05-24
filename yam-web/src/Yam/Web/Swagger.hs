{-# LANGUAGE NoPolyKinds #-}

module Yam.Web.Swagger(
    SwaggerConfig(..)
  , mkServeWithSwagger
  ) where

import           Control.Lens       hiding (Context)
import           Data.Aeson
import           Data.Aeson.Types   (typeMismatch)
import           Data.Default
import           Data.Maybe
import           Data.Reflection
import           Data.Swagger
import           Data.Text          (Text)
import           Data.Vault.Lazy
import           GHC.TypeLits
import           Network.Wai
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI

import           Yam.Web.Internal

data SwaggerUIType = Classic | Jensoleg deriving Show
data SwaggerConfig = SwaggerConfig
  { uiType        :: SwaggerUIType
  , uiPath        :: String
  , apiPath       :: String
  , enabled       :: Bool
  , apiTitle      :: Text
  , apiVersion    :: Text
  , contractName  :: Maybe Text
  , contractEmail :: Maybe Text
  } deriving Show

instance FromJSON SwaggerUIType where
  parseJSON v = go <$> parseJSON v
    where go :: Text -> SwaggerUIType
          go "default" = Classic
          go _         = Jensoleg

instance FromJSON SwaggerConfig where
  parseJSON (Object v) = SwaggerConfig
    <$> v .:? "type"    .!= Classic
    <*> v .:? "path"    .!= "swagger-ui.html"
    <*> v .:? "schema"  .!= "swagger.json"
    <*> v .:? "enabled" .!= True
    <*> v .:? "title"   .!= ""
    <*> v .:? "version" .!= "1.0.0"
    <*> v .:? "name"
    <*> v .:? "email"
  parseJSON invalid    = typeMismatch "SwaggerConfig" invalid

instance Default SwaggerConfig where
  def = fromJust $ decode "{}"

swagger :: (HasSwagger api) => SwaggerConfig -> Proxy (SwaggerSchemaUI dir schema :<|> api) -> Proxy api -> Server api -> Server (SwaggerSchemaUI dir schema :<|> api)
swagger conf _ proxy api = go (uiType conf) (f conf $ toSwagger proxy) :<|> api
    where go Jensoleg = jensolegSwaggerSchemaUIServer
          go _        = swaggerSchemaUIServer
          f SwaggerConfig{..} s = s
                & info.title       .~ apiTitle
                & info.version     .~ apiVersion
                & info.contact     ?~ Contact contractName Nothing contractEmail

mkServeWithSwagger :: (HasSwagger api, API api) => Vault -> [Middleware] -> SwaggerConfig -> Proxy api -> ServerT api App -> Application
mkServeWithSwagger vault middlewares conf proxy server =
  if enabled conf
    then reifyGroup conf $ \p -> mkServe' (\(p0,s0) -> (p,swagger conf p p0 s0)) vault middlewares proxy server
    else mkServe' id vault middlewares proxy server

reifyGroup :: SwaggerConfig -> (forall d s. (KnownSymbol d ,KnownSymbol s)=> Proxy (SwaggerSchemaUI d s :<|> api) -> r) -> r
reifyGroup SwaggerConfig{..} f = reifySymbol uiPath $ \pd -> reifySymbol apiPath $ \ps -> f $ group pd ps

group :: Proxy dir -> Proxy schema -> Proxy (SwaggerSchemaUI dir schema :<|> api)
group _ _ = Proxy
