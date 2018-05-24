module Yam.Web(
    module Yam.Web.Internal
  , module Yam.Web.Swagger
  , module Yam.Web.Middleware
  , YamSettings(..)
  , defaultYamSettings
  , runServer
  ) where

import           Yam.Config
import           Yam.Logger
import           Yam.Web.Internal         hiding (API', mkServe')
import           Yam.Web.Middleware
import           Yam.Web.Swagger

import           Data.Aeson
import           Data.Default
import           Data.Monoid              ((<>))
import           Data.Proxy
import           Data.Text                (pack)
import           Data.Vault.Lazy
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.Swagger

data YamSettings = YamSettings
  { vault       :: Vault
  , port        :: Int
  , middlewares :: [Middleware]
  , loggers     :: LoggerConfig
  , swaggers    :: SwaggerConfig
  }

instance Show YamSettings where
  show YamSettings{..}
    = "YAM Settings:"
    <> "\n port="      <> show port
    <> "\n log-level=" <> show (rank loggers)
    <> "\n swagger="   <> show swaggers
    <> "\n swagger-url=http://localhost:" <> show port <> "/" <> uiPath swaggers

defaultYamSettings :: IO YamSettings
defaultYamSettings = do
  c   <- defaultConfig
  f   <- getValueOrDef "app.yaml" "yam.conf" c
  v   <- merge' [return c, fromFile f False] :: IO Value
  sc  <- getValueOrDef def  "yam.swagger"   v :: IO SwaggerConfig
  rk  <- getValueOrDef INFO "yam.log.level" v
  pt  <- getValueOrDef 8080 "yam.port"      v
  lc' <- stdoutLoggerConfig
  let lc = lc' { rank = rk }
  debugLn lc $ "Load config file " <> pack f
  lm <- loggerMiddleware lc
  return $ YamSettings empty pt [lm, servantErrorMiddleware lc] lc sc

runServer :: (HasSwagger api, API api) => YamSettings -> Proxy api -> ServerT api App -> IO ()
runServer YamSettings{..} p a = run port $ mkServeWithSwagger vault middlewares swaggers p a
