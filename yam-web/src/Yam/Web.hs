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
  { port        :: Int
  , middlewares :: [Middleware]
  , loggers     :: LoggerConfig
  , swaggers    :: SwaggerConfig
  }

instance Show YamSettings where
  show YamSettings{..}
    = "YAM Settings:"
    <> "\n port="      <> show port
    <> "\n swagger="   <> show swaggers
    <> "\n swagger-url=http://localhost:" <> show port <> "/" <> uiPath swaggers

defaultYamSettings :: IO YamSettings
defaultYamSettings = do
  c        <- defaultConfig
  f        <- getValueOrDef "app.yaml" "yam.conf" c
  conf     <- merge' [return c, fromFile f False] :: IO Value
  swaggers <- getValueOrDef def  "yam.swagger"   conf :: IO SwaggerConfig
  rk       <- getValueOrDef INFO "yam.log.level" conf
  port     <- getValueOrDef 8080 "yam.port"      conf
  lc'      <- stdoutLoggerConfig
  let loggers = lc' { rank = rk }
      middlewares = [traceMiddleware $ traceKey loggers, apacheMiddleware loggers, servantErrorMiddleware loggers]
  debugLn loggers $ "Load config file " <> pack f
  return YamSettings{..}

runServer :: (HasSwagger api, API YamSettings api) => YamSettings -> Proxy api -> ServerT api App -> IO ()
runServer ys@YamSettings{..} p a = do
  run port $ mkServeWithSwagger empty Proxy ys middlewares swaggers p a
