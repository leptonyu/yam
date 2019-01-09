{-# LANGUAGE NoPolyKinds #-}
module Yam.Swagger(
    SwaggerConfig(..)
  , serveWithContextAndSwagger
  , module Control.Lens
  ) where

import           Control.Lens       hiding (Context, Empty, allOf, (.=))
import           Data.Reflection
import           Data.Swagger       hiding (name, port)
import qualified Data.Swagger       as S
import           GHC.TypeLits
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

type SAPI dir schema api = SwaggerSchemaUI dir schema :<|> api

serveWithContextAndSwagger
  :: (HasSwagger api, HasServer api context)
  => SwaggerConfig
  -> AppConfig
  -> Version
  -> Proxy api
  -> Context context
  -> ServerT api Handler
  -> Application
serveWithContextAndSwagger SwaggerConfig{..} AppConfig{..} versions proxy cxt api =
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
    g3 p a _ = swaggerSchemaUIServer (g4 $ toSwagger p) :<|> a
    g4 s = s
      & info .~ (mempty
          & title   .~ (name <> " API Documents")
          & S.version .~ pack (showVersion versions))
      & host ?~ Host "localhost" (Just $ fromIntegral port)
