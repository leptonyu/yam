module Yam.Types.Prelude(
    defJson
  , randomString
  , showText
  , readConfig
  , LogFunc
  , Default(..)
  , Text
  , pack
  , HasCallStack
  , MonadError(..)
  , MonadUnliftIO(..)
  , SomeException(..)
  , fromException
  , bracket
  , throw
  , try
  , module Data.Proxy
  , module Data.Vault.Lazy
  , module Data.Maybe
  , module Data.Aeson
  , module Data.Word
  , module Data.Text.Encoding
  , module Data.Function
  , module Data.Salak
  , module Data.Version
  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.Reader
  , module Control.Monad.Logger.CallStack
  , module Network.Wai
  , module Network.HTTP.Types
  ) where

import           Control.Applicative
import           Control.Exception              hiding (Handler)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Binary                    as B
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Base16.Lazy    as B16
import qualified Data.ByteString.Lazy           as L
import           Data.Default
import           Data.Function
import           Data.Maybe
import           Data.Proxy
import           Data.Salak
    ( FromProperties (..)
    , Properties
    , defaultPropertiesWithFile
    )
import qualified Data.Salak                     as S
import           Data.Text                      (Text, pack)
import           Data.Text.Encoding             (decodeUtf8, encodeUtf8)
import           Data.Vault.Lazy                (Key, Vault, newKey)
import           Data.Version
import           Data.Word
import           GHC.Stack
import           Network.HTTP.Types
import           Network.Wai
import           System.IO.Unsafe               (unsafePerformIO)
import           System.Random.MWC

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

defJson :: FromJSON a => a
defJson = fromJust $ decode "{}"

{-# NOINLINE randomGen #-}
randomGen :: GenIO
randomGen = unsafePerformIO create

-- | Utility
-- {-# INLINE randomString #-}
randomString :: IO ByteString
randomString = (L.toStrict . B16.encode . B.encode) <$> (uniform randomGen :: IO Word64)

{-# INLINE showText #-}
showText :: Show a => a -> Text
showText = pack . show

readConfig :: (Default a, FromProperties a) => Text -> Properties -> a
readConfig k p = fromMaybe def $ S.lookup k p
