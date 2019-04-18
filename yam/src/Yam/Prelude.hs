{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE UndecidableInstances #-}

module Yam.Prelude(
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
  , when
  , (<>)
  , LogLevel(..)
  , logInfo
  , logError
  , logWarn
  , logDebug
  , Loc(..)
  , MonadIO(..)
  , HasContextEntry(..)
  , TryContextEntry(..)
  , fromMaybe
  , (&)
  , decodeUtf8
  , encodeUtf8
  , fromJust
  , Version
  , Middleware
  ) where

import           Control.Exception                   hiding (Handler)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Data.Aeson
import qualified Data.Binary                         as B
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Base16.Lazy         as B16
import qualified Data.ByteString.Lazy                as L
import           Data.Default
import           Data.Function
import           Data.Maybe
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text, pack)
import           Data.Text.Encoding                  (decodeUtf8, encodeUtf8)
import qualified Data.Vector                         as V
import           Data.Version
import           Data.Word
import           GHC.Stack
import           Network.Wai
import           Servant
import           Servant.Server.Internal.ServerError
import           System.IO.Unsafe                    (unsafePerformIO)
import           System.Random.MWC

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

throwS :: (HasCallStack, MonadIO m, MonadLogger m) => ServerError -> Text -> m a
throwS e msg = do
  logErrorCS ?callStack msg
  liftIO $ throw e { errBody = encode $ WebErrResult msg}

whenException :: SomeException -> Response
whenException e = responseServerError $ fromMaybe err400 { errBody = encode $ WebErrResult $ showText e} (fromException e :: Maybe ServerError)

-- | Utility
randomCode :: V.Vector Char -> Int -> IO String
randomCode seed v = do
  let l = V.length seed
  vs <- replicateM v (uniformR (0, l - 1) randomGen)
  return $ (seed V.!) <$> vs


class TryContextEntry (cxt :: [*]) (entry :: *) where
  tryContextEntry :: Context cxt -> Maybe entry

instance TryContextEntry '[] entry where
  tryContextEntry _ = Nothing

instance {-# OVERLAPPABLE #-} TryContextEntry (entry ': as) entry where
  tryContextEntry (a :. _) = Just a

instance {-# OVERLAPPABLE #-} TryContextEntry as entry => TryContextEntry (a ': as) entry where
  tryContextEntry (_ :. as) = tryContextEntry as




