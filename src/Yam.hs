{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Yam(
    start
  , App
  , YamConfig(..)
  , DataSourceProvider
  , DataSourceConfig(..)
  , showText
  , throwServant
  , runDb
  , selectValue
  ) where

import           Control.Exception              (throw)
import           Control.Monad.Except
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Network.Wai
import           Network.Wai.Handler.Warp       (run)
import           Yam.Config
import           Yam.DataSource
import           Yam.Util
import           Yam.Web.Swagger

type AppM m = ReaderT (YamConfig, Maybe DataSource) (LoggingT m)

type App = AppM IO

throwServant :: ServantErr -> App a
throwServant = lift . throw

runDb :: DB App a -> App a
runDb a = do
  (_, ds) <- ask
  case ds of
    Nothing -> do
      logError "DataSource not found"
      throwServant err401
    Just d  -> runDB d a

start
  :: (HasSwagger api, HasServer api '[YamConfig])
  => YamConfig
  -> Proxy api
  -> ServerT api App
  -> [Middleware]
  -> Maybe DataSourceProvider
  -> App a
  -> LoggingT IO ()
start conf@YamConfig{..} proxy server middleWares newDs appa
  = let cxt = (conf :. EmptyContext)
  in do
    logInfo "Start Service..."
    runLogger <- askLoggerIO
    tryRunDb newDs datasource runLogger
      $ \ds -> do
        _ <- runLoggingT (runReaderT appa (conf,ds)) runLogger
        run port
          $ foldr (.) id middleWares
          $ serveWithContextAndSwagger swagger proxy cxt
          $ hoistServerWithContext proxy (Proxy :: Proxy '[YamConfig]) (go runLogger ds) server
  where
    go :: LogFunc -> Maybe DataSource -> App a -> Handler a
    go r ds a = liftIO $ (`runLoggingT` r) $ runReaderT a (conf, ds)
    tryRunDb (Just d) ds r a = do
      logInfo "Initialize Datasource..."
      liftIO $ runInDB r d ds (a.Just)
    tryRunDb _  _        _ a = liftIO $ a Nothing
