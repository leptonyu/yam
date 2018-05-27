module Yam.Logger(
    LogRank(..)
  , LoggerConfig(..)
  , LoggerMonad(..)
  , defaultLoggerConfig
  , stdoutLoggerConfig
  , toMonadLogger
  , addVaultToLoggerConfig
  , logger
  , logL
  , logLn
  , traceLn
  , debugLn
  , infoLn
  , warnLn
  , errorLn
  , toLogStr
  , (<>)
  ) where

import           Yam.Config.Vault

import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Monoid
import           Data.Text              (Text)
import           Data.Vault.Lazy
import           System.Log.FastLogger

data LogRank
  = TRACE
  | DEBUG
  | INFO
  | WARN
  | ERROR
  deriving (Show, Eq, Ord)

instance FromJSON LogRank where
  parseJSON v = go <$> parseJSON v
    where go :: Text -> LogRank
          go "trace" = TRACE
          go "debug" = DEBUG
          go "info"  = INFO
          go "warn"  = WARN
          go "error" = ERROR
          go  _      = INFO

type Logger = LogStr -> IO ()
type Clock  = IO FormattedTime

data LoggerConfig = LoggerConfig
  { func     :: Logger
  , clock    :: Clock
  , logKey   :: Key Text
  , traceKey :: Key Text
  , rank     :: LogRank
  , logVault :: Vault
  }

defaultLoggerConfig :: IO LoggerConfig
defaultLoggerConfig = do
  c <- newTimeCache "%F %X"
  k <- newKey
  t <- newKey
  return $ LoggerConfig (\_ -> return ()) c k t INFO empty

stdoutLoggerConfig :: IO LoggerConfig
stdoutLoggerConfig = do
  lc <- defaultLoggerConfig
  ls <- newStdoutLoggerSet 4096
  return lc { func = pushLogStr ls }

toRank :: LogLevel -> LogRank
toRank LevelDebug = DEBUG
toRank LevelInfo  = INFO
toRank LevelWarn  = WARN
toRank LevelError = ERROR
toRank _          = INFO

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

toMonadLogger :: LoggerConfig -> LogFunc
toMonadLogger lc@LoggerConfig{..} _ ls level = logger lc { logVault = addFirstVault ls "." logKey logVault} (toRank level)

addVaultToLoggerConfig :: Vault -> LoggerConfig -> LoggerConfig
addVaultToLoggerConfig vault lc = lc {logVault = union vault $ logVault lc}

logger :: LoggerConfig -> LogRank -> Logger
logger LoggerConfig{..} r str = when (r >= rank) $ do
  now <- clock
  let nm :: Text
      nm = extracBoxOrDefault "" $ newBox logKey logVault
      ti :: Text
      ti = extracBoxOrDefault "" (newBox traceKey logVault)
  func $ toLogStr now <> " [" <> toLogStr (show r) <> "] - [" <> toLogStr ti <> "] " <> toLogStr nm <> " - " <> str

class MonadIO m => LoggerMonad m where
  loggerConfig :: m LoggerConfig

logL :: (ToLogStr msg, LoggerMonad m) => LogRank -> msg -> m ()
logL rank msg = do
  lc <- loggerConfig
  liftIO $ logger lc rank $ toLogStr msg

logLn :: (ToLogStr msg, LoggerMonad m) => LogRank -> msg -> m ()
logLn rank msg = do
  lc <- loggerConfig
  liftIO $ logger lc rank $ toLogStr msg <> "\n"

traceLn :: (ToLogStr msg, LoggerMonad m) => msg -> m ()
traceLn = logLn TRACE
{-# INLINE traceLn #-}
debugLn :: (ToLogStr msg, LoggerMonad m) => msg -> m ()
debugLn = logLn DEBUG
{-# INLINE debugLn #-}
infoLn  :: (ToLogStr msg, LoggerMonad m) => msg -> m ()
infoLn  = logLn INFO
{-# INLINE infoLn #-}
warnLn  :: (ToLogStr msg, LoggerMonad m) => msg -> m ()
warnLn  = logLn WARN
{-# INLINE warnLn #-}
errorLn :: (ToLogStr msg, LoggerMonad m) => msg -> m ()
errorLn = logLn ERROR
{-# INLINE errorLn #-}
