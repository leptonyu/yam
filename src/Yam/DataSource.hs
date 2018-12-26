{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Yam.DataSource where

import           Control.Exception              (bracket)
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Data.Acquire                   (withAcquire)
import           Data.Aeson
import           Data.Conduit
import qualified Data.Conduit.List              as CL
import           Data.Pool
import           Data.Text                      (Text)
import           Database.Persist.Sql

data DataSourceConfig = DataSourceConfig
  { url        :: Text
  , maxThreads :: Int
  } deriving (Eq, Show)

instance FromJSON DataSourceConfig where
  parseJSON = withObject "DataSourceConfig" $ \v -> DataSourceConfig
    <$> v .: "url"
    <*> v .:? "max-threads" .!= 10

data DataSource = DataSource
  { config :: DataSourceConfig
  , pool   :: ConnectionPool
  }

type DataSourceProvider = DataSourceConfig -> LoggingT IO ConnectionPool

runInDB :: LogFunc -> DataSourceProvider -> DataSourceConfig -> (DataSource -> IO a) -> IO a
runInDB logfunc f config g = bracket (runLoggingT (f config) logfunc) destroyAllResources (\pool -> g DataSource{..})

-- SqlPersistT ~ ReaderT SqlBackend
type DB = SqlPersistT

runDB :: (MonadLoggerIO m, MonadUnliftIO m) => DataSource -> DB m a -> m a
runDB DataSource{..} db = do
  logger <- askLoggerIO
  withRunInIO $ \run -> withResource pool $ run . \c -> runSqlConn db c { connLogFunc = logger }

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
