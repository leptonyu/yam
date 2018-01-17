{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Yam.Logger(
    LogRank(..)
  , LoggerConfig(..)
  , MonadYamLogger(..)
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
  , toMonadLogger
  , toWaiLogger
  , runLoggingT
  ) where

import           Yam.Import

import qualified Control.Concurrent.Map     as M
import           Control.Monad.Catch        (bracket_)
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           GHC.Stack
import           Network.Wai.Logger
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

class MonadIO m => MonadYamLogger m where
  loggerConfig     :: m LoggerConfig
  withLoggerConfig :: LoggerConfig -> m a -> m a

instance (MonadIO m) => MonadYamLogger (ReaderT LoggerConfig m) where
  loggerConfig     = ask
  withLoggerConfig = withReaderT . const

logL :: (MonadYamLogger m, HasCallStack) => forall msg . (ToLogStr msg) => LogRank -> msg -> m ()
logL = logL' callStack

logL' :: MonadYamLogger m => forall msg . (ToLogStr msg) => CallStack -> LogRank -> msg -> m ()
logL' stack r msg = do
  conf    <- loggerConfig
  mayName <- fetchName
  liftIO $ when (r >= rank conf) $ do
    now      <- clock conf
    logger conf (cs now) (getName (getCallStack stack) `mergeMaybe` (cs <$> mayName)) r (toLogStr msg)
  where getName []          = Nothing
        getName ((_,loc):_) = Just $ (<>" ") $ cs $ srcLocModule loc

fetchName :: MonadYamLogger m => m (Maybe Text)
fetchName = do
  conf <- loggerConfig
  liftIO $ do
    threadId <- myThreadId
    M.lookup threadId $ name conf

setName :: MonadYamLogger m => Maybe Text -> m ()
setName m = do
  conf <- loggerConfig
  liftIO $ myThreadId >>= void . go m (name conf)
  where go (Just nm) cache tid = M.insert tid nm cache
        go _         cache tid = M.delete tid    cache

logLn :: (MonadYamLogger m, HasCallStack) =>  LogRank -> Text -> m ()
logLn = logLn' callStack

logLn' :: (MonadYamLogger m) => CallStack -> LogRank -> Text -> m ()
logLn' call l msg = logL' call l $ msg <> "\n"

traceLn :: (MonadYamLogger m, HasCallStack) => Text -> m ()
traceLn = logLn' callStack TRACE
{-# INLINE traceLn #-}
debugLn :: (MonadYamLogger m, HasCallStack) => Text -> m ()
debugLn = logLn' callStack DEBUG
{-# INLINE debugLn #-}
infoLn  :: (MonadYamLogger m, HasCallStack) => Text -> m ()
infoLn  = logLn' callStack INFO
{-# INLINE infoLn #-}
warnLn  :: (MonadYamLogger m, HasCallStack) => Text -> m ()
warnLn  = logLn' callStack WARN
{-# INLINE warnLn #-}
errorLn :: (MonadYamLogger m, HasCallStack) => Text -> m ()
errorLn = logLn' callStack ERROR
{-# INLINE errorLn #-}

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
        mkLogger fl time mayName rk msg = do
          thread  <- myThreadId
          let pre  = time
                  <> " ["
                  <> showText thread
                  <> "] "
                  <> showText rk
                  <> " "
                  <> fromMaybe "" mayName
                  <> " - "
          fl $ toLogStr pre <> msg

withLoggerName :: (MonadYamLogger m, MonadMask m) => Text -> m a -> m a
withLoggerName nm action = do
  mayName <- fetchName
  let mayName' = Just $ merge nm mayName
  bracket_ (setName mayName') (setName mayName) action
  where merge n (Just v) = v <> "." <> n
        merge n _        = n

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

toMonadLogger :: (MonadYamLogger m) => m LogFunc
toMonadLogger = mkLogger <$> loggerConfig
   where toRank LevelDebug = DEBUG
         toRank LevelInfo  = INFO
         toRank LevelWarn  = WARN
         toRank LevelError = ERROR
         toRank _          = INFO
         mkLogger :: LoggerConfig -> LogFunc
         mkLogger context = let go :: HasCallStack => LogFunc
                                go _ pre level msg = runReaderT (withLoggerName pre $ logL' callStack (toRank level) (msg <> "\n")) context
                            in go

toWaiLogger :: (MonadYamLogger m) => m ApacheLogger
toWaiLogger = do mkLogger <- flip runReaderT <$> loggerConfig
                 liftIO   $  apacheLogger
                         <$> initLogger FromFallback (LogCallback (mkLogger . logL' emptyCallStack INFO) $ return ()) (return "")
