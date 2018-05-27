{-# LANGUAGE NoPolyKinds #-}

module Yam.Web(
    module Yam.Web.Internal
  , module Yam.Web.Swagger
  , module Yam.Web.Middleware
  , YamSettings(..)
  , Yam
  , defaultYamSettings
  , runServer
  ) where

import           Yam.Config
import           Yam.Logger
import           Yam.Transaction
import           Yam.Transaction.Postgresql
import           Yam.Transaction.Sqlite
import           Yam.Web.Internal
import           Yam.Web.Middleware
import           Yam.Web.Swagger

import           Data.Aeson
import           Data.Default
import           Data.Monoid                ((<>))
import           Data.Proxy
import           Network.Wai
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Servant.Swagger

data YamSettings = YamSettings
  { port        :: Int
  , middlewares :: [Middleware]
  , loggers     :: LoggerConfig
  , swaggers    :: SwaggerConfig
  , dataSources :: Maybe DataSource
  }

instance Show YamSettings where
  show YamSettings{..}
    = "YAM Settings:"
    <> "\n  port="      <> show port
    <> "\n  swagger="   <> show swaggers
    <> "\n  swagger-url=http://localhost:" <> show port <> "/" <> uiPath swaggers

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
  dsc      <- getValueOrDef def "yam.datasource" conf :: IO DataSourceConfig
  dataSources <- if Yam.Transaction.enabled dsc
          then Just <$> dataSource loggers dsc [sqliteProvider,postgresqlProvider]
          else return Nothing
  logger loggers DEBUG $ "Load config file " <> toLogStr f <> "\n"
  return YamSettings{..}

type Yam = App YamSettings

instance LoggerMonad Yam where
  loggerConfig = do
    (req,YamSettings{..}) <- ask
    return loggers {logVault = vault req}

runServer :: (HasSwagger api, HasServer api '[YamSettings]) => YamSettings -> Proxy api -> ServerT api Yam -> IO ()
runServer ys@YamSettings{..} p a = run port $ mkServeWithSwagger p Proxy (ys :. EmptyContext) Proxy ys middlewares swaggers a



