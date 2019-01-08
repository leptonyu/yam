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

import qualified Data.Text             as T
import           GHC.Stack
import           Servant
import           System.IO.Unsafe      (unsafePerformIO)
import           System.Log.FastLogger
import           Yam.Types.Env
import           Yam.Types.Prelude

instance FromJSON LogLevel where
  parseJSON v = go . T.toLower <$> parseJSON v
    where
      go :: Text -> LogLevel
      go "debug" = LevelDebug
      go "info"  = LevelInfo
      go "warn"  = LevelWarn
      go "error" = LevelError
      go level   = LevelOther level

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
  def = defJson

instance FromJSON LogConfig where
  parseJSON = withObject "LogConfig" $ \v -> LogConfig
    <$> v .:? "buffer-size" .!= 4096
    <*> v .:? "file"        .!= ""
    <*> v .:? "max-size"    .!= 10485760
    <*> v .:? "max-history" .!= 256
    <*> v .:? "level"       .!= LevelInfo

newLogger :: Text -> LogConfig -> IO (LogFunc, IO ())
newLogger name LogConfig{..} = do
  tc        <- newTimeCache "%Y-%m-%d %T"
  let ft = if file == ""
            then LogStdout $ fromIntegral bufferSize
            else LogFile (FileLogSpec file (toInteger maxSize) (fromIntegral rotateHistory)) $ fromIntegral bufferSize
  (l,close) <- newTimedFastLogger tc ft
  return (toLogger l, close)
  where
    toLogger f Loc{..} _ ll s = when (level <= ll) $ f $ \t ->
      let locate = if ll /= LevelError then "" else " @" <> toLogStr loc_filename <> toLogStr (show loc_start)
      in toLogStr t <> " " <> toStr ll <> " [" <> toLogStr name <> "] " <> toLogStr loc_module <> locate <> " - " <> s <> "\n"

withLogger :: Text -> LogConfig -> LoggingT IO a -> IO a
withLogger n lc action = bracket (newLogger n lc) snd $ \(f,_) -> runLoggingT action f

addTrace :: LogFunc -> Text -> LogFunc
addTrace f tid a b c d = f a b c ("[" <> toLogStr tid <> "] " <> d)

{-# NOINLINE loggerKey #-}
loggerKey :: Key LogFunc
loggerKey = unsafePerformIO newKey

{-# NOINLINE extensionLogKey #-}
extensionLogKey :: Key Text
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

throwS :: (HasCallStack, MonadIO m, MonadLogger m) => ServantErr -> Text -> m a
throwS e msg = do
  logErrorCS ?callStack msg
  liftIO $ throw e
