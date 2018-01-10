{-# LANGUAGE OverloadedStrings #-}

module Yam.Logger.MonadLogger(
     toMonadLogger
   , runLoggingT
   ) where

import           Yam.Import
import           Yam.Logger

import           Control.Monad.Logger

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

toMonadLogger :: (Yam.Logger.MonadLogger m) => m LogFunc
toMonadLogger = mkLogger <$> loggerConfig
   where toRank LevelDebug = DEBUG
         toRank LevelInfo  = INFO
         toRank LevelWarn  = WARN
         toRank LevelError = ERROR
         toRank _          = INFO
         mkLogger :: LoggerConfig -> LogFunc
         mkLogger context  _ name level msg = runReaderT (withLoggerName name $ logL (toRank level) (msg <> "\n")) context
