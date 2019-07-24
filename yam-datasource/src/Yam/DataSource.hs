-- |
-- Module:      Yam.DataSource
-- Copyright:   (c) 2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Datasource supports for [yam](https://hackage.haskell.org/package/yam).
--
module Yam.DataSource(
  -- * DataSource Types
    DataSourceProvider(..)
  , DataSource
  , DB
  , HasDataSource
  , DataSourceConfig(..)
  , runTrans
  , datasourceMiddleware
  -- * Sql Functions
  , query
  , selectValue
  ) where

import           Control.Exception              (bracket)
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import qualified Control.Monad.State            as MS
import           Data.Acquire                   (withAcquire)
import           Data.Conduit
import qualified Data.Conduit.List              as CL
import           Data.Default
import           Data.Pool
import qualified Data.Text                      as T
import           Database.Persist.Sql           hiding (Key)
import           Salak
import           Yam
import           Yam.Middleware


data DataSourceConfig = DataSourceConfig
  { dsType  :: T.Text
  , dsUrl   :: T.Text
  , maxConn :: Int
  } deriving Show


instance Default DataSourceConfig where
  def = DataSourceConfig "sqlite" ":memory:" 10

instance MonadCatch m => FromProp m DataSourceConfig where
  fromProp = DataSourceConfig
    <$> "type"            .?: dsType
    <*> "url"             .?: dsUrl
    <*> "max-connections" .?: maxConn

-- | Middleware context type.
type DataSource = Pool SqlBackend

data DataSourceProvider = DataSourceProvider
  { datasource :: LoggingT IO DataSource
  , migration  :: DB (LoggingT IO) ()
  , dbtype     :: T.Text
  -- , check      :: DataSource -> IO HealthStatus
  }
-- SqlPersistT ~ ReaderT SqlBackend
type DB = SqlPersistT

query
  :: (MonadUnliftIO m)
  => T.Text
  -> [PersistValue]
  -> DB m [[PersistValue]]
query sql params = do
  res <- rawQueryRes sql params
  withAcquire res (\a -> runConduit $ a .| CL.fold (flip (:)) [])

selectValue :: (PersistField a, MonadUnliftIO m) => T.Text -> DB m [a]
selectValue sql = fmap unSingle <$> rawSql sql []

-- | Middleware context.
type HasDataSource cxt = (HasBase cxt, HasCxt cxt DataSource)

runTrans
  :: ( HasDataSource cxt
     , MonadIO m
     , MonadUnliftIO m
     , MonadThrow m)
  => DB (AppT cxt m) a
  -> AppT cxt m a
runTrans a = do
  pool   <- askCxt
  logger <- askLoggerIO
  withRunInIO $ \run -> withResource pool $ run . \c -> runSqlConn a c { connLogFunc = logger }

datasourceMiddleware :: HasBase cxt => DataSourceProvider -> AppMiddleware IO amtdcxt cxt DataSource
datasourceMiddleware DataSourceProvider{..}
  = AppMiddleware $ \f -> do
    logInfo "datasource started"
    amtd <- MS.get
    cxt  <- ask
    lf   <- askLoggerIO
    amt2 <- liftIO
      $ bracket (runLoggingT datasource lf) destroyAllResources
      $ \ds -> snd <$> runAMT amtd cxt (f ds)
    MS.put amt2


 -- \c m h f -> askLoggerIO >>= \lc ->
 --  liftIO $ bracket
 --    (runLoggingT datasource lc)
 --    destroyAllResources
 --    (\ds -> runLoggingT (f (ds :. c) m (mergeHealth (check ds) "datasource" h)) lc)



