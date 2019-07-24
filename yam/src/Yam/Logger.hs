{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yam.Logger(
  -- * Logger Function
    withLogger
  , getLogger
  , extensionLogKey
  , liftX
  , LogConfig(..)
  , HasLogger
  , LogFuncHolder(..)
  , VaultHolder(..)
  ) where
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Data.Text                      (unpack)
import qualified Data.Vault.Lazy                as L
import           Data.Word
import           Salak
import           System.IO.Unsafe               (unsafePerformIO)
import           System.Log.FastLogger
import           Yam.Prelude

instance MonadThrow m => FromProp m LogLevel where
  fromProp = readEnum fromEnumProp
    where
      fromEnumProp "debug" = Right   LevelDebug
      fromEnumProp "info"  = Right   LevelInfo
      fromEnumProp "warn"  = Right   LevelWarn
      fromEnumProp "error" = Right   LevelError
      fromEnumProp u       = Left $ "unknown level: " ++ unpack u

{-# INLINE toStr #-}
toStr :: LogLevel -> LogStr
toStr LevelDebug     = "DEBUG"
toStr LevelInfo      = " INFO"
toStr LevelWarn      = " WARN"
toStr LevelError     = "ERROR"
toStr (LevelOther l) = toLogStr l

-- | Logger config
data LogConfig = LogConfig
  { bufferSize    :: Word16   -- ^ Logger buffer size.
  , file          :: FilePath -- ^ Logger file path.
  , maxSize       :: Word32   -- ^ Max logger file size.
  , rotateHistory :: Word16   -- ^ Max number of logger files should be reserved.
  , level         :: IO LogLevel -- ^ Log level to show.
  }
instance Default LogConfig where
  def = LogConfig 4096 "" 10485760 256 (return LevelInfo)

instance (MonadIO m, MonadCatch m) => FromProp m LogConfig where
  fromProp = LogConfig
    <$> "buffer-size" .?: bufferSize
    <*> "file"        .?: file
    <*> "max-size"    .?: maxSize
    <*> "max-history" .?: rotateHistory
    <*> "level"       .?: level

newLogger :: Text -> LogConfig -> IO (LogFunc, IO ())
newLogger name LogConfig{..} = do
  tc            <- newTimeCache "%Y-%m-%d %T"
  let ft = if file == ""
            then LogStdout $ fromIntegral bufferSize
            else LogFile (FileLogSpec file (toInteger maxSize) (fromIntegral rotateHistory)) $ fromIntegral bufferSize
      ln = " [" <> toLogStr name <> "] "
  (l,close) <- newTimedFastLogger tc ft
  return (toLogger ln l, close)
  where
    toLogger xn f Loc{..} _ ll s = do
      lc <- level
      when (lc <= ll) $ f $ \t ->
        let locate = if ll /= LevelError then "" else " @" <> toLogStr loc_filename <> toLogStr (show loc_start)
        in toLogStr t <> " " <> toStr ll <> xn <> toLogStr loc_module <> locate <> " - " <> s <> "\n"


liftX :: MonadIO m => LoggingT IO () -> LoggingT m ()
liftX f = askLoggerIO >>= liftIO . runLoggingT f

withLogger :: (MonadUnliftIO m) => Text -> LogConfig -> (LogFunc -> LoggingT m a) -> m a
withLogger n lc action = do
  f <- askRunInIO
  liftIO $ bracket (newLogger n lc) snd $ f . runLoggingT (askLoggerIO >>= action) . fst

addTrace :: LogFunc -> Text -> LogFunc
addTrace f tid a b c d = let p = "[" <> toLogStr tid <> "] " in f a b c (p <> d)

{-# NOINLINE extensionLogKey #-}
extensionLogKey :: L.Key Text
extensionLogKey = unsafePerformIO L.newKey

getLogger :: Maybe VaultHolder -> LogFuncHolder -> LogFunc
getLogger (Just (VH vault)) (LF logger) =
  let {-# INLINE nlf #-}
      nlf x (Just t) = addTrace x t
      nlf x _        = x
  in nlf logger $ L.lookup extensionLogKey vault
getLogger _ (LF logger) = logger

-- | Holder for 'LogFunc'
newtype LogFuncHolder = LF LogFunc
-- | Holder for 'Vault'
newtype VaultHolder   = VH L.Vault

-- | Context with logger.
type HasLogger cxt = (HasContextEntry cxt LogFuncHolder, TryContextEntry cxt VaultHolder)

