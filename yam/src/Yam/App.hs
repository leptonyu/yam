{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
module Yam.App where

import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Data.Menshen
import           Salak
import           Servant
import           Yam.Logger
import           Yam.Prelude

newtype AppT cxt m a = AppT { runAppT' :: ReaderT (Context cxt) m a } deriving (Functor, Applicative, Monad)

type App cxt = AppT cxt IO

instance MonadTrans (AppT cxt) where
  lift = AppT . lift

instance MonadIO m => MonadIO (AppT cxt m) where
  liftIO = AppT . liftIO

instance Monad m => MonadReader (Context cxt) (AppT cxt m) where
  ask = AppT ask
  local f (AppT a) = AppT $ local f a

instance MonadUnliftIO m => MonadUnliftIO (AppT cxt m) where
  askUnliftIO = do
    cxt <- ask
    uio <- lift askUnliftIO
    return (UnliftIO $ unliftIO uio . runAppT cxt)

instance (HasLogger cxt, MonadIO m) => HasValid (AppT cxt m) where
  invalid a = throwS err400 (pack $ toI18n a)

instance (HasLogger cxt, MonadIO m) => MonadLogger (AppT cxt m) where
  monadLoggerLog a b c d = do
    f <- getEntry
    v <- tryEntry
    liftIO $ getLogger v f a b c (toLogStr d)

instance (HasLogger cxt, MonadIO m) => MonadLoggerIO (AppT cxt m) where
  askLoggerIO = do
    f <- getEntry
    v <- tryEntry
    return (getLogger v f)

getEntry :: (HasContextEntry cxt entry, Monad m) => AppT cxt m entry
getEntry = asks getContextEntry

tryEntry :: (TryContextEntry cxt entry, Monad m) => AppT cxt m (Maybe entry)
tryEntry = asks tryContextEntry

runAppT :: Context cxt -> AppT cxt m a -> m a
runAppT c a = runReaderT (runAppT' a) c

instance (HasContextEntry cxt SourcePack, Monad m) => HasSourcePack (AppT cxt m) where
  askSourcePack = getEntry

runVault :: (cxt' ~ (Vault ': cxt), MonadIO m) => Context cxt -> Vault -> App cxt' a -> m a
runVault c v a = liftIO $ runAppT (v :. c) a

nt :: cxt' ~ (Vault ': cxt) => Context cxt -> Vault -> App cxt' a -> Handler a
nt = runVault
