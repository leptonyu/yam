{-# LANGUAGE ImplicitParams #-}
module Yam.Logger(
  -- * Logger Function
    withLogger
  , putLogger
  , setExtendLog
  , getLogger
  , extensionLogKey
  , throwS
  , LogConfig(..)
  ) where

import           Salak
import qualified Data.Vault.Lazy       as L
import           System.IO.Unsafe      (unsafePerformIO)
import           System.Log.FastLogger
import           Yam.Types.Env
import           Yam.Types.Prelude

instance FromEnumProp LogLevel where
  fromEnumProp "debug" = Right   LevelDebug
  fromEnumProp "info"  = Right   LevelInfo
  fromEnumProp "warn"  = Right   LevelWarn
  fromEnumProp "error" = Right   LevelError
  fromEnumProp _       = Right $ LevelOther "fatal"

{-# INLINE toStr #-}
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
  } deriving (Eq, Show)

instance Default LogConfig where
  def = LogConfig 4096 "" 10485760 256 LevelInfo

instance FromProp LogConfig where
  fromProp = LogConfig
    <$> "buffer-size" .?: bufferSize
    <*> "file"        .?: file
    <*> "max-size"    .?: maxSize
    <*> "max-history" .?: rotateHistory
    <*> "level"       .?: level

newLogger :: Text -> IO LogConfig -> IO (LogFunc, IO ())
newLogger name lc = do
  LogConfig{..} <- lc
  tc            <- newTimeCache "%Y-%m-%d %T"
  let ft = if file == ""
            then LogStdout $ fromIntegral bufferSize
            else LogFile (FileLogSpec file (toInteger maxSize) (fromIntegral rotateHistory)) $ fromIntegral bufferSize
      ln = " [" <> toLogStr name <> "] "
  (l,close) <- newTimedFastLogger tc ft
  return (toLogger ln l, close)
  where
    toLogger xn f Loc{..} _ ll s = do
      c <- lc
      when (level c <= ll) $ f $ \t ->
        let locate = if ll /= LevelError then "" else " @" <> toLogStr loc_filename <> toLogStr (show loc_start)
        in toLogStr t <> " " <> toStr ll <> xn <> toLogStr loc_module <> locate <> " - " <> s <> "\n"

withLogger :: Text -> IO LogConfig -> LoggingT IO a -> IO a
withLogger n lc action = bracket (newLogger n lc) snd $ \(f,_) -> runLoggingT action f

addTrace :: LogFunc -> Text -> LogFunc
addTrace f tid a b c d = let p = "[" <> toLogStr tid <> "] " in f a b c (p <> d)

{-# NOINLINE loggerKey #-}
loggerKey :: L.Key LogFunc
loggerKey = unsafePerformIO newKey

{-# NOINLINE extensionLogKey #-}
extensionLogKey :: L.Key Text
extensionLogKey = unsafePerformIO newKey

setExtendLog :: (Text -> Text) -> Env -> Env
setExtendLog f env = let mt = fromMaybe "" $ getAttr extensionLogKey env in setAttr extensionLogKey (f mt) env

putLogger :: LogFunc -> Env -> Env
putLogger = setAttr loggerKey

getLogger :: Env -> LogFunc
getLogger env =
  let trace  :: Maybe Text    = getAttr extensionLogKey  env
      logger :: Maybe LogFunc = getAttr loggerKey env
      {-# INLINE nlf #-}
      nlf x (Just t) = addTrace x t
      nlf x _        = x
  in maybe (\_ _ _ _ -> return ()) (`nlf` trace) logger
