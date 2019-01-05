module Yam.DataSource(
    DataSourceConfig(..)
  , DataSourceProvider
  , query
  , selectValue
  , runTrans
  , datasourceMiddleware
  ) where

import           Control.Exception       (bracket)
import           Control.Monad.IO.Unlift
import           Data.Acquire            (withAcquire)
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.Pool
import           Database.Persist.Sql    hiding (Key)
import           System.IO.Unsafe        (unsafePerformIO)
import           Yam.Types               hiding (LogFunc)

data DataSourceConfig = DataSourceConfig
  { url        :: Text
  , maxThreads :: Int
  } deriving (Eq, Show)

instance FromJSON DataSourceConfig where
  parseJSON = withObject "DataSourceConfig" $ \v -> DataSourceConfig
    <$> v .:? "url"         .!= ":memory:"
    <*> v .:? "max-threads" .!= 10

instance Default DataSourceConfig where
  def = defJson

data DataSource = DataSource
  { config :: DataSourceConfig
  , pool   :: ConnectionPool
  }

{-# NOINLINE dataSourceKey #-}
dataSourceKey :: Key DataSource
dataSourceKey = unsafePerformIO newKey

type DataSourceProvider = LoggingT IO ConnectionPool
-- SqlPersistT ~ ReaderT SqlBackend
type DB = SqlPersistT

query
  :: (MonadUnliftIO m)
  => Text
  -> [PersistValue]
  -> DB m [[PersistValue]]
query sql params = do
  res <- rawQueryRes sql params
  withAcquire res (\a -> runConduit $ a .| CL.fold (flip (:)) [])

selectValue :: (PersistField a, MonadUnliftIO m) => Text -> DB m [a]
selectValue sql = fmap unSingle <$> rawSql sql []

runTrans :: (MonadUnliftIO m) => DB (AppM m) a -> AppM m a
runTrans a = requireAttr dataSourceKey >>= (`runDB` a)

{-# INLINE runDB #-}
runDB :: (MonadLoggerIO m, MonadUnliftIO m) => DataSource -> DB m a -> m a
runDB DataSource{..} db = do
  logger <- askLoggerIO
  withRunInIO $ \run -> withResource pool $ run . \c -> runSqlConn db c { connLogFunc = logger }

{-# INLINE runInDB #-}
runInDB :: LogFunc -> DataSourceProvider -> DataSourceConfig -> (DataSource -> IO a) -> IO a
runInDB logfunc f config g = bracket (runLoggingT f logfunc) destroyAllResources (\pool -> g DataSource{..})

datasourceMiddleware :: DataSourceConfig -> DataSourceProvider -> AppMiddleware
datasourceMiddleware dsc dsp = AppMiddleware $ \env f -> do
  lf <- askLoggerIO
  logInfo "Datasource Initialized..."
  liftIO $ runInDB lf dsp dsc $ \ds -> runLoggingT (f (setAttr dataSourceKey ds env, id)) lf

