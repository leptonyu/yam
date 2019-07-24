{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
module Yam.Internal.App where

import           Control.Monad.Catch     hiding (Handler)
import           Control.Monad.Except
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Salak
import           Yam.Internal.Types

newtype AppT cxt m a = AppT { unAppT :: ReaderT cxt (ExceptT SomeException m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadThrow (AppT cxt m) where
  throwM = AppT . lift . throwError . toException

instance Monad m => MonadCatch (AppT cxt m) where
  catch ma f = do
    v <- try ma
    case v of
      Left  e -> f e
      Right x -> return x

instance MonadTrans (AppT cxt) where
  lift = AppT . lift . lift

instance Monad m => MonadReader cxt (AppT cxt m) where
  ask = AppT ask
  local f (AppT a) = AppT $ local f a

instance (MonadThrow m, MonadUnliftIO m) => MonadUnliftIO (AppT cxt m) where
  askUnliftIO = do
    cxt <- ask
    uio <- lift askUnliftIO
    return (UnliftIO $ unliftIO uio . runAppT cxt)

runAppT :: MonadThrow m => cxt -> AppT cxt m a -> m a
runAppT cxt (AppT ma) = do
  v <- runExceptT $ runReaderT ma cxt
  case v of
    Left  e -> throwM e
    Right x -> return x

instance (HasSalak cxt, Monad m) => MonadSalak (AppT cxt m) where
  askSalak = askCxt
