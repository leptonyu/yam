{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE UndecidableInstances #-}

module Yam.Types.Prelude(
  -- * Utilities
    randomString
  , showText
  , throwS
  , randomCode
  , whenException
  -- * Reexport Functions
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
  , (<>)
  , module Data.Proxy
  , module Data.Vault.Lazy
  , module Data.Maybe
  , module Data.Word
  , module Data.Text.Encoding
  , module Data.Function
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
import           Data.Aeson
import qualified Data.Binary                        as B
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Base16.Lazy        as B16
import qualified Data.ByteString.Lazy               as L
import           Data.Default
import           Data.Function
import           Data.Maybe
import           Data.Monoid                        ((<>))
import           Data.Proxy
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
import           System.IO.Unsafe                   (unsafePerformIO)
import           System.Random.MWC
#if MIN_VERSION_servant_server(0,16,0)
import           Servant.Server.Internal.ServerError
type ServantErr = ServerError
responseServantErr = responseServerError
#else
import           Servant.Server.Internal.ServantErr
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

data WebErrResult = WebErrResult
  { message :: Text
  }

instance ToJSON WebErrResult where
  toJSON WebErrResult{..} = object [ "message" .= message ]

throwS :: (HasCallStack, MonadIO m, MonadLogger m) => ServantErr -> Text -> m a
throwS e msg = do
  logErrorCS ?callStack msg
  liftIO $ throw e { errBody = encode $ WebErrResult msg}

whenException :: SomeException -> Response
whenException e = responseServantErr $ fromMaybe err400 (fromException e :: Maybe ServantErr)

-- | Utility
randomCode :: V.Vector Char -> Int -> IO String
randomCode seed v = do
  let l = V.length seed
  vs <- sequence $  replicate v $ uniformR (0,l-1) randomGen
  return $ (seed V.!) <$> vs
