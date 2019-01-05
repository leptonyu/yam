{-# LANGUAGE NoPolyKinds #-}
module Yam.Swagger where

import           Data.Reflection
import           GHC.TypeLits
import           Servant.Swagger
import           Servant.Swagger.UI
import           Yam.Types

data SwaggerConfig = SwaggerConfig
  { urlDir    :: String
  , urlSchema :: String
  , enabled   :: Bool
  } deriving (Eq, Show)

instance Default SwaggerConfig where
  def = defJson

instance FromJSON SwaggerConfig where
  parseJSON = withObject "SwaggerConfig" $ \v -> SwaggerConfig
    <$> v .:? "dir"     .!= "swagger-ui"
    <*> v .:? "schema"  .!= "swagger-ui.json"
    <*> v .:? "enabled" .!= True

type SAPI dir schema api = SwaggerSchemaUI dir schema :<|> api

serveWithContextAndSwagger
  :: (HasSwagger api, HasServer api context)
  => SwaggerConfig
  -> Proxy api
  -> Context context
  -> ServerT api Handler
  -> Application
serveWithContextAndSwagger SwaggerConfig{..} proxy cxt api =
    if enabled
      then reifySymbol urlDir $ \pd -> reifySymbol urlSchema $ \ps -> go (pd,ps) proxy cxt api
      else serveWithContext proxy cxt api
  where
    go :: (HasSwagger api, HasServer api context, KnownSymbol d, KnownSymbol s)
        => (Proxy d, Proxy s)
        -> Proxy api
        -> Context context
        -> ServerT api Handler
        -> Application
    go pds p c api' = let p' = g2 pds in serveWithContext p' c (g3 p api' p')
    g2 :: (Proxy d, Proxy s) -> Proxy (SAPI d s api)
    g2 _ = Proxy
    g3 :: HasSwagger api => Proxy api -> Server api -> Proxy (SAPI d s api) -> Server (SAPI d s api)
    g3 p a _ = swaggerSchemaUIServer (toSwagger p) :<|> a
