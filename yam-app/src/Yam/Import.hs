module Yam.Import(
    Text
  , pack
  , cs
  , showText
  , lift
  , join
  , MonadIO
  , liftIO
  , when
  , unless
  , forM_
  , void
  , (<>)
  , myThreadId
  , ThreadId
  , killThread
  , fromMaybe
  , maybe
  , mapMaybe
  , catMaybes
  , selectMaybe
  , isNothing
  , isJust
  , finally
  , MonadMask
  , MonadThrow
  , MonadCatch
  , catchAll
  , runReaderT
  , ReaderT
  , ask
  , Generic
  , UTCTime
  , addUTCTime
  , fromTime
  , millisToUTC
  , randomHex
  , Proxy(..)
  , encodeToText
  , FromJSON(..)
  , ToJSON(..)
  , decode
  , Default(..)
  , MonadBaseControl
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Data.Aeson
import           Data.Default
import           Data.Maybe
import           Data.Monoid                 ((<>))
import           Data.Proxy
import           Data.String.Conversions     (cs)
import           Data.Text                   (Text, pack)
import           Data.Time                   (UTCTime)
import           Data.Time.Clock             (addUTCTime)
import           Data.Time.Clock.POSIX       (posixSecondsToUTCTime)
import           Data.Time.Format            (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime         (utcToLocalZonedTime)
import           GHC.Generics
import           System.Random               (newStdGen, randoms)


millisToUTC :: Integer -> UTCTime
millisToUTC t = posixSecondsToUTCTime $ fromInteger t / 1000

fromTime :: Text -> UTCTime -> IO Text
fromTime p t = do zt <- utcToLocalZonedTime t
                  return $ cs $ formatTime defaultTimeLocale (cs p) zt

encodeToText :: ToJSON e => e -> Text
encodeToText = cs . encode

showText :: Show a => a -> Text
showText = cs . show

_hex = ['0'..'9'] <> ['a'..'f']

randomHex :: Int -> IO Text
randomHex n = (pack . map (go _hex 16) . take n . randoms) <$> newStdGen
          where go gs l v = gs !! mod v l

selectMaybe :: [Maybe a] -> Maybe a
selectMaybe = listToMaybe . catMaybes
