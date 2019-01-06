module Yam.DataSource(
  -- * DataSource Types
    DataSourceProvider
  , DataSource
  , DB
  -- * Primary DataSource Functions
  , runTrans
  , primaryDatasourceMiddleware
  -- * Secondary DataSource Functions
  , runTransWith
  , datasourceMiddleware
  -- * Sql Functions
  , query
  , selectValue
  ) where

import           Control.Exception       (bracket)
import           Control.Monad.IO.Unlift
import           Data.Acquire            (withAcquire)
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.Pool
import           Database.Persist.Sql    hiding (Key)
import           System.IO.Unsafe        (unsafePerformIO)
import           Yam                     hiding (LogFunc)

type DataSource = ConnectionPool

{-# NOINLINE dataSourceKey #-}
dataSourceKey :: Key DataSource
dataSourceKey = unsafePerformIO newKey

type DataSourceProvider = LoggingT IO DataSource
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

runTransWith :: (MonadUnliftIO m) => Key DataSource -> DB (AppM m) a -> AppM m a
runTransWith k a = requireAttr k >>= (`runDB` a)

runTrans :: (MonadUnliftIO m) => DB (AppM m) a -> AppM m a
runTrans = runTransWith dataSourceKey

{-# INLINE runDB #-}
runDB :: (MonadLoggerIO m, MonadUnliftIO m) => DataSource -> DB m a -> m a
runDB pool db = do
  logger <- askLoggerIO
  withRunInIO $ \run -> withResource pool $ run . \c -> runSqlConn db c { connLogFunc = logger }

{-# INLINE runInDB #-}
runInDB :: LogFunc -> DataSourceProvider -> (DataSource -> IO a) -> IO a
runInDB logfunc f g = bracket (runLoggingT f logfunc) destroyAllResources g

datasourceMiddleware :: Key DataSource -> DataSourceProvider -> AppMiddleware
datasourceMiddleware k dsp = AppMiddleware $ \env f -> do
  lf <- askLoggerIO
  logInfo "Datasource Initialized..."
  liftIO $ runInDB lf dsp $ \ds -> runLoggingT (f (setAttr k ds env, id)) lf

primaryDatasourceMiddleware = datasourceMiddleware dataSourceKey
