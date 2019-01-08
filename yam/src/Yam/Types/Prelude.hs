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
  , module Control.Monad
  , module Control.Monad.Reader
  , module Control.Monad.Logger.CallStack
  , module Network.Wai
  , module Network.HTTP.Types
  ) where

import           Control.Exception              hiding (Handler)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Data.Aeson
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
import           Data.Text                      (Text, justifyRight, pack)
import           Data.Text.Encoding             (decodeUtf8, encodeUtf8)
import           Data.Vault.Lazy                (Key, Vault, newKey)
import           Data.Version
import           Data.Word
import           GHC.Stack
import           Network.HTTP.Types
import           Network.Wai
import           Numeric
import           System.Random

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

defJson :: FromJSON a => a
defJson = fromJust $ decode "{}"

-- | Utility
{-# INLINE randomString #-}
randomString :: Int -> IO Text
randomString n = do
  c <- randomIO :: IO Word64
  return $ justifyRight n '0' $ pack $ take n $ showHex c ""

{-# INLINE showText #-}
showText :: Show a => a -> Text
showText = pack . show

readConfig :: (Default a, FromProperties a) => Text -> Properties -> a
readConfig k p = fromMaybe def $ S.lookup k p
