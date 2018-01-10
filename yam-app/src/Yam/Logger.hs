{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Yam.Logger(
    LogRank(..)
  , LoggerConfig(..)
  , MonadLogger(..)
  , logL
  , logLn
  , errorLn
  , warnLn
  , infoLn
  , debugLn
  , traceLn
  , stdoutLogger
  , fileLogger
  , withLoggerName
  ) where

import           Yam.Import

import qualified Control.Concurrent.Map     as M
import           Control.Monad.Catch        (bracket_)
import           Control.Monad.Trans.Reader
import           System.Log.FastLogger

data LogRank = TRACE
             | DEBUG
             | INFO
             | WARN
             | ERROR
             deriving (Show,Eq,Ord)

instance FromJSON LogRank where
  parseJSON v = go <$> parseJSON v
    where go :: Text -> LogRank
          go "trace" = TRACE
          go "debug" = DEBUG
          go "info"  = INFO
          go "warn"  = WARN
          go "error" = ERROR
          go  _      = INFO

type LoggerCache = M.Map ThreadId Text
type LoggerFunc  = Text -> Maybe Text -> LogRank -> LogStr -> IO ()
data LoggerConfig = LoggerConfig
   { logger :: LoggerFunc
   , clock  :: IO FormattedTime
   , rank   :: LogRank
   , name   :: LoggerCache
   }

class (MonadIO m) => MonadLogger m where
  loggerConfig     :: m LoggerConfig
  withLoggerConfig :: LoggerConfig -> m a -> m a

instance (MonadIO m) => MonadLogger (ReaderT LoggerConfig m) where
  loggerConfig     = ask
  withLoggerConfig = withReaderT . const

logL :: (MonadLogger m) => forall msg . (ToLogStr msg) => LogRank -> msg -> m ()
logL r msg = do
  conf    <- loggerConfig
  mayName <- fetchName
  liftIO $ when (r >= rank conf) $ do
    now      <- clock conf
    logger conf (cs now) (cs <$> mayName) r (toLogStr msg)

fetchName :: MonadLogger m => m (Maybe Text)
fetchName = do
  conf <- loggerConfig
  liftIO $ do
    threadId <- myThreadId
    M.lookup threadId $ name conf

setName :: MonadLogger m => Maybe Text -> m ()
setName m = do
  conf <- loggerConfig
  liftIO $ myThreadId >>= void . go m (name conf)
  where go (Just m) cache tid = M.insert tid m cache
        go _        cache tid = M.delete tid   cache

logLn :: (MonadLogger m) => LogRank -> Text -> m ()
logLn l msg = logL l $ msg <> "\n"

traceLn :: (MonadLogger m) => Text -> m ()
traceLn = logLn TRACE
debugLn :: (MonadLogger m) => Text -> m ()
debugLn = logLn DEBUG
infoLn  :: (MonadLogger m) => Text -> m ()
infoLn  = logLn INFO
warnLn  :: (MonadLogger m) => Text -> m ()
warnLn  = logLn WARN
errorLn :: (MonadLogger m) => Text -> m ()
errorLn = logLn ERROR

defaultLoggerConfig :: LoggerFunc -> IO LoggerConfig
defaultLoggerConfig func = do
     nm        <- M.empty
     timeCache <- newTimeCache "%F %X"
     return $ LoggerConfig func timeCache DEBUG nm

stdoutLogger :: IO LoggerConfig
stdoutLogger = newStdoutLoggerSet 4096 >>= newLog

fileLogger :: FilePath -> IO LoggerConfig
fileLogger file = newFileLoggerSet 4096 file >>= newLog

newLog :: LoggerSet -> IO LoggerConfig
newLog = defaultLoggerConfig . mkLogger . pushLogStr
  where mkLogger :: FastLogger -> LoggerFunc
        mkLogger logger time mayName rank msg = do
          thread  <- myThreadId
          let name = time
                  <> " ["
                  <> showText thread
                  <> "] "
                  <> showText rank
                  <> " "
                  <> fromMaybe "" mayName
                  <> " - "
          logger $ toLogStr name <> msg

withLoggerName :: (MonadLogger m, MonadMask m) => Text -> m a -> m a
withLoggerName nm action = do
  mayName <- fetchName
  let mayName' = Just $ merge nm mayName
  bracket_ (setName mayName') (setName mayName) action
  where merge n (Just v) = v <> "." <> n
        merge n _        = n

withLogger :: (MonadLogger m) => (LoggerConfig -> LoggerConfig) -> m a -> m a
withLogger modify action = do
  conf    <- loggerConfig
  withLoggerConfig (modify conf) action


