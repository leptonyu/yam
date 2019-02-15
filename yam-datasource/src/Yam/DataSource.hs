module Yam.DataSource(
  -- * DataSource Types
    DataSourceProvider(..)
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

import           Control.Monad.IO.Unlift
import           Data.Acquire            (withAcquire)
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.Pool
import           Database.Persist.Sql    hiding (Key)
import           System.IO.Unsafe        (unsafePerformIO)
import           Yam                     hiding (LogFunc)

type DataSource = Pool SqlBackend

{-# NOINLINE dataSourceKey #-}
dataSourceKey :: Key DataSource
dataSourceKey = unsafePerformIO newKey

data DataSourceProvider = DataSourceProvider
  { datasource :: LoggingT IO DataSource
  , migration  :: DB (LoggingT IO) ()
  , dbtype     :: Text
  }
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

runTransWith :: Key DataSource -> DB App a -> App a
runTransWith k a = requireAttr k >>= (`runDB` a)

runTrans :: DB App a -> App a
runTrans = runTransWith dataSourceKey

{-# INLINE runDB #-}
runDB :: (MonadLoggerIO m, MonadUnliftIO m) => DataSource -> DB m a -> m a
runDB pool db = do
  logger <- askLoggerIO
  withRunInIO $ \run -> withResource pool $ run . \c -> runSqlConn db c { connLogFunc = logger }

datasourceMiddleware :: Key DataSource -> DataSourceProvider -> AppMiddleware
datasourceMiddleware k DataSourceProvider{..} = simplePoolMiddleware (True, "database " <> dbtype) k open (liftIO . destroyAllResources)
  where
    {-# INLINE trans #-}
    trans :: LoggingT IO a -> App a
    trans a = askLoggerIO >>= liftIO . runLoggingT a
    {-# INLINE open #-}
    open = do
      a <- trans datasource
      trans $ runDB a migration
      return a

primaryDatasourceMiddleware = datasourceMiddleware dataSourceKey



