{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Yam.App(
    module Yam.App.Context
  , module Yam.Import
  , module Yam.Event
  , module Yam.Logger
  , module Yam.Prop
  , module Yam.Transaction
  , runAppM
  , AppM
  , RunMode(..)
  , defaultContext
  , registerEventHandler
  , registerEventHandler'
  , evalPropOrDefault
  , evalProp
  , enable
  ) where

import           Yam.App.Context
import           Yam.Event
import           Yam.Import
import           Yam.Logger
import           Yam.Prop
import           Yam.Transaction

import           Control.Monad.Trans.Control (MonadBaseControl)


type AppM = ReaderT YamContext

data RunMode = Development | Production deriving (Show, Eq)

instance FromJSON RunMode where
  parseJSON v = go <$> parseJSON v
    where go :: Text -> RunMode
          go "production" = Production
          go _            = Development

runAppM :: (Monad m) => YamContext -> AppM m a -> m a
runAppM = flip runReaderT

instance (MonadIO m, MonadThrow m) => HasYamContext (AppM m) where
  yamContext = ask

defaultContext :: IO YamContext
defaultContext = do
  context  <- emptyContext
  runAppM context $ do
    loadProps
    initLogger

loadProps :: AppM IO ()
loadProps = do
  context <- ask
  let showLog :: IO PropertySource -> IO PropertySource
      showLog a = do
        src@(f,_) <- a
        runAppM context $ debugLn $ "Load Config " <> f <> " .."
        return src
  source <- liftIO $ do
    cmdSource <- showLog loadCommandLineArgs
    envSource <- showLog loadEnv
    let baseSource@(_,v) = mergePropertySource [cmdSource, envSource]
    mayConf   <- runProp v $ getProp "config"
    case mayConf of
      Nothing -> return baseSource
      Just c  -> do
        confSource@(_,cv) <- showLog $ loadYaml c
        configs           <- runProp cv $ getPropOrDefault [] "configs"
        addtionalSource   <- mapM (showLog . loadYaml) configs
        return $ mergePropertySource $ baseSource:confSource:addtionalSource
  setExtension keyProp source

initLogger :: AppM IO YamContext
initLogger = do
  mayLogFile <- getProp "log.file"
  logRank    <- getPropOrDefault DEBUG "log.level"
  context    <- ask
  let config = defLogger context
  case mayLogFile of
    Just file -> do
      newLogger <- liftIO $ fileLogger file
      setExtension keyLogger $ newLogger { rank = logRank }
      return context
    Nothing   -> return context { defLogger = config {rank = logRank} }

enable :: FromJSON a => Text -> Bool -> Text -> (Maybe a -> AppM IO ()) -> AppM IO ()
enable keyEnable def key action = do
        enables <- getPropOrDefault def keyEnable
        when enables $ getProp key >>= action

keyLogger :: Text
keyLogger = "Extension.Logger"

instance (MonadIO m, MonadThrow m) => MonadYamLogger (AppM m) where
  loggerConfig     = do
    context <- yamContext
    getExtensionOrDefault (defLogger context) keyLogger
  withLoggerConfig = (>>) . setExtension keyLogger

keyProp :: Text
keyProp = "Extension.Prop"

instance (MonadIO m, MonadThrow m) => MonadProp (AppM m) where
  propertySource = requireExtension keyProp

evalProp :: FromJSON a => YamContext -> Text -> IO (Maybe a)
evalProp c = runAppM c . getProp

evalPropOrDefault :: FromJSON a => a -> YamContext -> Text -> IO a
evalPropOrDefault a c key = fromMaybe a <$> evalProp c key

keyTransaction :: Text
keyTransaction = "Extension.Transaction"
keySecondaryTransaction :: Text
keySecondaryTransaction = "Extension.Transaction.Secondary"

instance (MonadIO m, MonadBaseControl IO m, MonadMask m) => MonadTransaction (AppM m) where
  connectionPool    = requireExtension keyTransaction
  secondaryPool     = getExtension     keySecondaryTransaction
  setConnectionPool p s = do
    setExtension keyTransaction p
    forM_ s (setExtension keySecondaryTransaction)

keyEvent :: Text
keyEvent = "Extension.Event."

instance (MonadIO m, MonadThrow m) => MonadEvent (AppM m) where
  eventHandler proxy = getExtensionOrDefault [] $ keyEvent <> cs (eventKey proxy)

registerEventHandler :: (MonadIO m, MonadThrow m, Event e) => Proxy e -> (e -> AppM IO ()) -> AppM m ()
registerEventHandler p = registerEventHandler' p Nothing

registerEventHandler' :: (MonadIO m, MonadThrow m, Event e) => Proxy e -> Maybe Text -> (e -> AppM IO ()) -> AppM m ()
registerEventHandler' p hname h = do
  hs      <- eventHandler p
  context <- ask
  let key  = keyEvent <> cs (eventKey p)
      h'   = runAppM context . h
      name = fromMaybe (key <> "." <> showText (length hs + 1)) hname
  infoLn $ "Register eventHandler " <> name <> " for " <> key
  setExtension key (h':hs)
