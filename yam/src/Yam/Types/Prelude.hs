{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE UndecidableInstances #-}
module Yam.Types.Prelude(
    randomString
  , showText
  , throwS
  , randomCode
  , whenException
#if !MIN_VERSION_salak(0,2,1)
  , (.>>)
  , (.?>)
  , (.?=)
  , (.|=)
#endif
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
  , catch
  , module Data.Proxy
  , module Data.Vault.Lazy
  , module Data.Maybe
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
import           Control.Exception                  hiding (Handler)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import qualified Data.Binary                        as B
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Base16.Lazy        as B16
import qualified Data.ByteString.Lazy               as L
import           Data.Default
import           Data.Function
import           Data.Maybe
import           Data.Proxy
import           Data.Salak
    ( FromProperties (..)
    , Properties
    , Property (..)
    , Return
    , defaultPropertiesWithFile
    )
import           Data.Text                          (Text, pack)
import           Data.Text.Encoding                 (decodeUtf8, encodeUtf8)
import           Data.Vault.Lazy                    (Key, Vault, newKey)
import qualified Data.Vector                        as V
import           Data.Version
import           Data.Word
import           GHC.Stack
import           Network.HTTP.Types
import           Network.Wai
import           Servant
import           Servant.Server.Internal.ServantErr
import           System.IO.Unsafe                   (unsafePerformIO)
import           System.Random.MWC

#if MIN_VERSION_salak(0,2,1)
import           Data.Salak                         ((.>>), (.?=), (.?>), (.|=))
#else
import qualified Data.Salak                         as S
import           Data.Text                          (unpack)

infixl 5 .?>
(.?>) :: FromProperties a => Properties -> Text -> Return a
(.?>) = flip S.lookup'

infixl 5 .|=
(.|=) :: Return a -> a -> a
(.|=) (S.OK   a) _ = a
(.|=) (S.Fail e) _ = error e
(.|=) _        d   = d

infixl 5 .?=
(.?=) :: Return a -> a -> Return a
(.?=) a b = S.OK (a .|= b)

infixl 5 .>>
(.>>) :: FromProperties a => Properties -> Text -> a
(.>>) p k = case p .?> k of
  S.OK v   -> v
  S.Empty  -> case fromProperties S.empty of
    (S.OK   a) -> a
    (S.Fail e) -> error e
    _          -> error $ "Config " <> unpack k <> " not found"
  S.Fail e -> error e
#endif

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

{-# NOINLINE randomGen #-}
randomGen :: GenIO
randomGen = unsafePerformIO createSystemRandom

-- | Utility
randomString :: IO ByteString
randomString = L.toStrict . B16.encode . B.encode <$> (uniform randomGen :: IO Word64)

{-# INLINE showText #-}
showText :: Show a => a -> Text
showText = pack . show

throwS :: (HasCallStack, MonadIO m, MonadLogger m) => ServantErr -> Text -> m a
throwS e msg = do
  logErrorCS ?callStack msg
  liftIO $ throw e


whenException :: SomeException -> Response
whenException e = responseServantErr $ fromMaybe err400 (fromException e :: Maybe ServantErr)

-- | Utility
randomCode :: V.Vector Char -> Int -> IO String
randomCode seed v = do
  let l = V.length seed
  vs <- sequence $  replicate v $ uniformR (0,l-1) randomGen
  return $ (seed V.!) <$> vs
