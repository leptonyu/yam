module Yam.Logger(
    LogRank(..)
  , Logger
  , LoggerConfig(..)
  , LogFunc
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
  ) where

import           Yam.Config.Vault

import           Control.Monad           (when)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Monoid
import           Data.String.Conversions (cs)
import           Data.Text               (Text, pack)
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

logger :: LoggerConfig -> Vault -> LogRank -> Logger
logger LoggerConfig{..} v r str = when (r >= rank) $ do
  now <- clock
  let v' = union v logVault
      nm = extracBoxOrDefault "" $ newBox logKey v'
      ti = "[" <> extracBoxOrDefault "" (newBox traceKey v') <> "]"
  func $ toLogStr (cs now <> " [" <> pack (show r) <> "] - " <> ti <> " " <> nm <> " - ") <> str

toRank :: LogLevel -> LogRank
toRank LevelDebug = DEBUG
toRank LevelInfo  = INFO
toRank LevelWarn  = WARN
toRank LevelError = ERROR
toRank _          = INFO

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

toMonadLogger :: LoggerConfig -> LogFunc
toMonadLogger lc@LoggerConfig{..} _ ls level = logger lc {logVault = addFirstVault ls "." logKey logVault} empty (toRank level)

addVaultToLoggerConfig :: Vault -> LoggerConfig -> LoggerConfig
addVaultToLoggerConfig vault lc = lc {logVault = union vault $ logVault lc}

logL :: forall msg . (ToLogStr msg) => LoggerConfig -> LogRank -> msg -> IO ()
logL lc rank = logger lc empty rank . toLogStr

logLn :: LoggerConfig -> LogRank -> Text -> IO ()
logLn lc rank msg = logL lc rank $ msg <> "\n"

traceLn :: MonadIO m => LoggerConfig -> Text -> m ()
traceLn lc = liftIO . logLn lc TRACE
{-# INLINE traceLn #-}
debugLn :: MonadIO m => LoggerConfig -> Text -> m ()
debugLn lc = liftIO . logLn lc DEBUG
{-# INLINE debugLn #-}
infoLn  :: MonadIO m => LoggerConfig -> Text -> m ()
infoLn  lc = liftIO . logLn lc INFO
{-# INLINE infoLn #-}
warnLn  :: MonadIO m => LoggerConfig -> Text -> m ()
warnLn  lc = liftIO . logLn lc WARN
{-# INLINE warnLn #-}
errorLn :: MonadIO m => LoggerConfig -> Text -> m ()
errorLn lc = liftIO . logLn lc ERROR
{-# INLINE errorLn #-}
