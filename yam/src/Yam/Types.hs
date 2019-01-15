module Yam.Types(
  -- * App Monad
    App
  , runApp
  , runTestApp
  , askApp
  , askAttr
  , withAttr
  , requireAttr
  , module Yam.Types.Env
  , module Yam.Types.Prelude
  ) where

import           Yam.Logger
import           Yam.Types.Env
import           Yam.Types.Prelude

newtype App a = App { runApp' :: ReaderT Env IO a } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env)

runApp :: MonadIO m => Env -> App a -> m a
runApp e a = liftIO $ runReaderT (runApp' a) e

runTestApp :: Key a -> a -> App b -> IO b
runTestApp k v a = runApp def $ withAttr k v a

instance MonadUnliftIO App where
  withRunInIO f = do
    env <- ask
    App $ withRunInIO (\g -> f $ g . lift . runApp env)

instance MonadLogger App where
  monadLoggerLog a b c d = do
    env <- ask
    liftIO $ getLogger env a b c $ toLogStr d

instance MonadLoggerIO App where
  askLoggerIO = asks getLogger

askApp :: App AppConfig
askApp = asks application

requireAttr :: Key a -> App a
requireAttr k = fromJust <$> askAttr k

askAttr :: Key a -> App (Maybe a)
askAttr = asks . getAttr

withAttr :: Key a -> a -> App b -> App b
withAttr k v = local (setAttr k v)


