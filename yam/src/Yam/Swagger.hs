{-# LANGUAGE NoPolyKinds #-}
module Yam.Swagger(
    SwaggerConfig(..)
  , serveWithContextAndSwagger
  ) where

import           Control.Lens       hiding (Context, Empty, allOf, (.=))
import           Data.Reflection
import           Data.Salak
import           Data.Swagger       hiding (name, port)
import qualified Data.Swagger       as S
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI
import           Yam.Types.Env
import           Yam.Types.Prelude

data SwaggerConfig = SwaggerConfig
  { urlDir    :: String
  , urlSchema :: String
  , enabled   :: Bool
  } deriving (Eq, Show)

instance Default SwaggerConfig where
  def = SwaggerConfig "swagger-ui" "swagger-ui.json" True

instance FromProperties SwaggerConfig where
  fromProperties p = SwaggerConfig
    <$> p .?> "dir"     .?= urlDir    def
    <*> p .?> "schema"  .?= urlSchema def
    <*> p .?> "enabled" .?= enabled   def

serveWithContextAndSwagger
  :: forall api context. (HasSwagger api, HasServer api context)
  => SwaggerConfig
  -> AppConfig
  -> Version
  -> Proxy api
  -> Context context
  -> ServerT api Handler
  -> Application
serveWithContextAndSwagger SwaggerConfig{..} AppConfig{..} versions proxy cxt api =
  if enabled
    then reifySymbol urlDir $ \pd -> reifySymbol urlSchema $ \ps -> 
         serveWithContext (go proxy pd ps) cxt (swaggerSchemaUIServer (g4 $ toSwagger proxy) :<|> api) 
    else serveWithContext proxy cxt api
  where
    go :: forall dir schema. Proxy api -> Proxy dir -> Proxy schema -> Proxy (SwaggerSchemaUI dir schema :<|> api)
    go _ _ _ = Proxy
    g4 s = s
      & info .~ (mempty
          & title   .~ (name <> " API Documents")
          & S.version .~ pack (showVersion versions))
      & host ?~ Host "localhost" (Just $ fromIntegral port)
