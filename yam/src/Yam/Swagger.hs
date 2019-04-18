{-# LANGUAGE NoPolyKinds #-}
module Yam.Swagger(
    SwaggerConfig(..)
  , serveWithContextAndSwagger
  , baseInfo
  ) where

import           Control.Lens       hiding (Context)
import           Data.Reflection
import           Data.Swagger
import           Data.Version       (showVersion)
import           Salak
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI
import           Yam.Prelude

data SwaggerConfig = SwaggerConfig
  { urlDir    :: String
  , urlSchema :: String
  , enabled   :: Bool
  } deriving (Eq, Show)

instance FromProp SwaggerConfig where
  fromProp = SwaggerConfig
    <$> "dir"     .?= "swagger-ui"
    <*> "schema"  .?= "swagger-ui.json"
    <*> "enabled" .?= True

serveWithContextAndSwagger
  :: forall api context. (HasSwagger api, HasServer api context)
  => SwaggerConfig
  -> (Swagger -> Swagger)
  -> Proxy api
  -> Context context
  -> ServerT api Handler
  -> Application
serveWithContextAndSwagger SwaggerConfig{..} g5 proxy cxt api =
  if enabled
    then reifySymbol urlDir $ \pd -> reifySymbol urlSchema $ \ps ->
         serveWithContext (go proxy pd ps) cxt (swaggerSchemaUIServer (g5 $ toSwagger proxy) :<|> api)
    else serveWithContext proxy cxt api
  where
    go :: forall dir schema. Proxy api -> Proxy dir -> Proxy schema -> Proxy (SwaggerSchemaUI dir schema :<|> api)
    go _ _ _ = Proxy

baseInfo :: Text -> Version -> Int -> Swagger -> Swagger
baseInfo n v p s = s
  & info . title   .~ n
  & info . version .~ pack (showVersion v)
  & host ?~ Host "localhost" (Just $ fromIntegral p)



