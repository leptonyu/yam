{-# LANGUAGE NoPolyKinds #-}

module Yam.Web(
    module Yam.Web.Internal
  , module Yam.Web.Swagger
  , module Yam.Web.Middleware
  , YamSettings(..)
  , Yam
  , defaultYamSettings
  , runDb
  , runDbIO
  , runServer
  ) where

import           Yam.Config
import           Yam.Config.Vault
import           Yam.Logger
import           Yam.Transaction
import           Yam.Transaction.Postgresql
import           Yam.Transaction.Sqlite
import           Yam.Web.Internal
import           Yam.Web.Middleware
import           Yam.Web.Swagger

import           Control.Exception          (finally, throw)
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
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
    <> "\n  datasource=" <> show dataSources

defaultYamSettings :: IO YamSettings
defaultYamSettings = do
  c        <- defaultConfig
  f        <- getValueOrDef "app.yaml" "yam.conf" c
  conf     <- merge' [return c, fromFile f False] :: IO Value
  swaggers <- getValueOrDef def  "yam.swagger"   conf :: IO SwaggerConfig
  rk       <- getValueOrDef INFO "yam.log.level" conf
  port     <- getValueOrDef 8080 "yam.port"      conf
  lc'      <- stdoutLoggerConfig
  gtc      <- randomString
  let loggers = lc' { rank = rk , logVault = addFirstVault ("[" <> gtc <> "]") "." (logKey lc') $ logVault lc'}
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

instance MonadUnliftIO Handler where
  askUnliftIO = return $ UnliftIO go
    where
      go (Handler rh) = do
        result <- runExceptT rh
        case result of
          Left  e -> throw e
          Right r -> return r

runDb :: Transaction Yam a -> Yam a
runDb r = do
  (_,YamSettings{..}) <- ask
  case dataSources of
    Nothing -> error $ "Datasource disabled"
    Just ds -> runTrans ds r

runDbIO :: YamSettings -> Transaction (ReaderT LoggerConfig IO) () -> IO ()
runDbIO YamSettings{..} r = case dataSources of
  Nothing -> return ()
  Just v  -> runReaderT (runTrans v r) loggers

runServer :: (HasSwagger api, HasServer api '[YamSettings]) => YamSettings -> Proxy api -> ServerT api Yam -> IO ()
runServer ys@YamSettings{..} p a = run port (mkServeWithSwagger p Proxy (ys :. EmptyContext) Proxy ys middlewares swaggers a) `finally` go loggers dataSources
  where
    go l = \case
      Nothing -> return ()
      Just d  -> closeDataSource l d


