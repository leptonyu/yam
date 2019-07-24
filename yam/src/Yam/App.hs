{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
module Yam.App where

import           Control.Monad.Catch            hiding (Handler)
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Data.Menshen
import           Salak
import           Servant
import           Yam.Logger
import           Yam.Prelude                    hiding (catch)

-- | Application Context Monad.
newtype AppT cxt m a = AppT { runAppT' :: ReaderT (Context cxt) m a } deriving (Functor, Applicative, Monad)

-- | Application on IO.
type AppIO cxt = AppT cxt IO

-- | Application with 'Vault'
type AppV cxt = AppT (VaultHolder : cxt)

-- | Application with 'SourcePack'
type AppS cxt = AppV (SourcePack : cxt)

instance MonadThrow m => MonadThrow (AppT cxt m) where
  throwM = AppT . lift . throwM

instance MonadTrans (AppT cxt) where
  lift = AppT . lift

instance MonadIO m => MonadIO (AppT cxt m) where
  liftIO = AppT . liftIO

instance Monad m => MonadReader (Context cxt) (AppT cxt m) where
  ask = AppT ask
  local f (AppT a) = AppT $ local f a

instance MonadCatch m => MonadCatch (AppT cxt m) where
  catch (AppT m) f = do
    c <- ask
    lift $ runReaderT m c `catch` (\e -> runReaderT (runAppT' $ f e) c)

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

-- | Get entry from 'AppT'
getEntry :: (HasContextEntry cxt entry, Monad m) => AppT cxt m entry
getEntry = asks getContextEntry

-- | Try get entry from 'AppT'
tryEntry :: (TryContextEntry cxt entry, Monad m) => AppT cxt m (Maybe entry)
tryEntry = asks tryContextEntry

-- | Run Application with context.
runAppT :: Context cxt -> AppT cxt m a -> m a
runAppT c a = runReaderT (runAppT' a) c

instance (HasContextEntry cxt SourcePack, Monad m) => MonadSalak (AppT cxt m) where
  askSalak = getEntry

type HasSalaks cxt = HasContextEntry cxt SourcePack

-- | Run Application with 'Vault'.
runVault :: MonadIO m => Context cxt -> Vault -> AppV cxt IO a -> m a
runVault c v a = liftIO $ runAppT (VH v :. c) a

nt :: Context cxt -> Vault -> AppV cxt IO a -> Handler a
nt = runVault
