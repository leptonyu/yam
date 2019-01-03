{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
module Yam.Logger where

import           Codec.Compression.GZip         ()
import           Control.Exception              (bracket)
import           Control.Monad                  (when)
import           Control.Monad.Logger.CallStack
import           Data.Aeson
import           Data.Default
import           Data.Maybe
import           Data.Text                      (Text, toLower)
import           Data.Word
import           System.Log.FastLogger

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

instance FromJSON LogLevel where
  parseJSON v = go . toLower <$> parseJSON v
    where
      go :: Text -> LogLevel
      go "debug" = LevelDebug
      go "info"  = LevelInfo
      go "warn"  = LevelWarn
      go "error" = LevelError
      go level   = LevelOther level

toStr :: LogLevel -> LogStr
toStr LevelDebug     = "DEBUG"
toStr LevelInfo      = " INFO"
toStr LevelWarn      = " WARN"
toStr LevelError     = "ERROR"
toStr (LevelOther l) = toLogStr l

data LogConfig = LogConfig
  { bufferSize    :: Word16
  , file          :: FilePath
  , maxSize       :: Word32
  , rotateHistory :: Word16
  , level         :: LogLevel
  } deriving Show

instance Default LogConfig where
  def = fromJust $ decode "{}"

instance FromJSON LogConfig where
  parseJSON = withObject "LogConfig" $ \v -> LogConfig
    <$> v .:? "buffer-size" .!= 4096
    <*> v .:? "file"        .!= ""
    <*> v .:? "max-size"    .!= 10485760
    <*> v .:? "max-history" .!= 256
    <*> v .:? "level"       .!= LevelInfo

newLogger :: LogConfig -> IO (LogFunc, IO ())
newLogger LogConfig{..} = do
  tc        <- newTimeCache "%Y-%m-%d %T"
  let ft = if file == ""
            then LogStdout $ fromIntegral bufferSize
            else LogFile (FileLogSpec file (toInteger maxSize) (fromIntegral rotateHistory)) $ fromIntegral bufferSize
  (l,close) <- newTimedFastLogger tc ft
  return (toLogger l, close)
  where
    toLogger f Loc{..} _ ll s = when (level <= ll) $ f $ \t ->
      let locate = if ll /= LevelError then "" else "@" <> toLogStr loc_filename <> toLogStr (show loc_start)
      in toLogStr t <> " " <> toStr ll <> " " <> toLogStr loc_module <> " " <> locate <> " - " <> s <> "\n"

withLogger :: LogConfig -> LoggingT IO a -> IO a
withLogger lc action = bracket (newLogger lc) snd $ \(f,_) -> runLoggingT action f

addTrace :: LogFunc -> Text -> LogFunc
addTrace f tid a b c d = f a b c ("[" <> toLogStr tid <> "] " <> d)
