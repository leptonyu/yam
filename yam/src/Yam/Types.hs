{-# LANGUAGE ImplicitParams #-}
module Yam.Types(
  -- * Environment
    AppConfig(..)
  , Env(..)
  , getAttr
  , setAttr
  , reqAttr
  -- * AppM Monad
  , AppM
  , runAppM
  , withAppM
  , App
  , askApp
  , askAttr
  , withAttr
  , requireAttr
  -- * Application Middleware
  , AppMiddleware(..)
  , simpleAppMiddleware
  , simpleWebMiddleware
  -- * Utilities
  , LogFunc
  , randomString
  , showText
  , defJson
  -- * Reexport Functions
  , Key
  , newKey
  , Middleware
  , Request(..)
  , lift
  , when
  , Default(..)
  , Text
  , pack
  , encodeUtf8
  , decodeUtf8
  , MonadIO
  , liftIO
  , withReaderT
  , module Control.Monad.Logger.CallStack
  , module Data.Maybe
  , module Servant
  , module Data.Aeson
  , module Data.Word
  , module Data.Function
  , module Data.Version
  ) where

import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Default
import           Data.Function
import           Data.Maybe
import           Data.Text                      (Text, justifyRight, pack)
import           Data.Text.Encoding             (decodeUtf8, encodeUtf8)
import           Data.Vault.Lazy                (Key, newKey)
import qualified Data.Vault.Lazy                as L
import           Data.Version
import           Data.Word
import           GHC.Stack
import           Network.Wai
import           Numeric
import           Servant
import           System.Random

data AppConfig = AppConfig
  { name :: Text
  , port :: Int
  } deriving (Eq, Show)

instance FromJSON AppConfig where
  parseJSON = withObject "AppConfig" $ \v -> AppConfig
    <$> v .:? "name" .!= "application"
    <*> v .:? "port" .!= 8888

defJson :: FromJSON a => a
defJson = fromJust $ decode "{}"

instance Default AppConfig where
  def = defJson

data Env = Env
  { attributes    :: Vault
  , reqAttributes :: Maybe Vault
  , application   :: AppConfig
  }

instance Default Env where
  def = Env L.empty Nothing def

getAttr :: Key a -> Env -> Maybe a
getAttr k Env{..} = listToMaybe $ catMaybes $ L.lookup k <$> catMaybes [reqAttributes, Just attributes]

reqAttr :: Default a => Key a -> Env -> a
reqAttr k = fromMaybe def . getAttr k

setAttr :: Key a -> a -> Env -> Env
setAttr k v Env{..} = case reqAttributes of
  Just av -> Env attributes (Just $ L.insert k v av)     application
  _       -> Env (L.insert k v attributes) reqAttributes application

newtype AppM m a = AppM { runAppM' :: ReaderT Env m a } deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)
type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()
type App = AppM IO

runAppM :: Env -> AppM m a -> m a
runAppM e a = runReaderT (runAppM' a) e

instance Monad m => MonadReader Env (AppM m) where
  ask = AppM ask
  local f (AppM a) = AppM $ local f a

instance MonadUnliftIO m => MonadUnliftIO (AppM m) where
  withRunInIO f = do
    env <- ask
    lift $ withRunInIO (\g -> f $ g . runAppM env)

withAppM :: MonadIO m => (Env -> Env) -> AppM m a -> AppM m a
withAppM f a = do
  env <- ask
  lift $ runAppM (f env) a

askApp :: Monad m => AppM m AppConfig
askApp = asks application

requireAttr :: MonadIO m => Key a -> AppM m a
requireAttr k = fromJust <$> askAttr k

askAttr :: MonadIO m => Key a -> AppM m (Maybe a)
askAttr = asks . getAttr

withAttr :: MonadIO m => Key a -> a -> AppM m b -> AppM m b
withAttr k v = withAppM (setAttr k v)

-- | Application Middleware
newtype AppMiddleware = AppMiddleware {runAM :: Env -> ((Env, Middleware)-> LoggingT IO ()) -> LoggingT IO ()}

instance Semigroup AppMiddleware where
  (AppMiddleware am) <> (AppMiddleware bm) = AppMiddleware $ \e f -> am e $ \(e', mw) -> bm e' $ \(e'',mw') -> f (e'', mw . mw')

instance Monoid AppMiddleware where
  mempty = AppMiddleware $ \a f -> f (a,id)

-- | Simple AppMiddleware
simpleAppMiddleware :: HasCallStack => (Bool, Text) -> Key a -> a -> AppMiddleware
simpleAppMiddleware (enabled,amname) k v =
  if enabled
    then AppMiddleware $ \e f -> do
      logInfoCS ?callStack $ amname <> " enabled"
      f (setAttr k v e, id)
    else mempty

simpleWebMiddleware :: HasCallStack => (Bool, Text) -> Middleware -> AppMiddleware
simpleWebMiddleware (enabled,amname) m =
  if enabled
    then AppMiddleware $ \e f -> do
      logInfoCS ?callStack $ amname <> " enabled"
      f (e,m)
    else mempty

-- | Utility
{-# INLINE randomString #-}
randomString :: Int -> IO Text
randomString n = do
  c <- randomIO :: IO Word64
  return $ justifyRight n '0' $ pack $ take n $ showHex c ""

{-# INLINE showText #-}
showText :: Show a => a -> Text
showText = pack . show

