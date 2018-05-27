{-# LANGUAGE UndecidableInstances #-}

module Yam.Transaction(
    Transaction
  , DataSourceConfig(..)
  , DataSourceProvider(..)
  , dataSource
  , closeDataSource
  , DataSource
  , runTrans
  , query
  , selectValue
  , selectNow
  ) where

import           Yam.Logger

import           Control.Monad.IO.Unlift    (MonadUnliftIO, withRunInIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader
import           Data.Acquire               (withAcquire)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import           Data.Default
import           Data.Either                (rights)
import           Data.Maybe                 (fromJust)
import           Data.Monoid                ((<>))
import           Data.Pool
import           Data.Pool                  (withResource)
import           Data.Text                  (Text, intercalate, unpack)
import           Data.Time                  (UTCTime)
import           Database.Persist.Sql

-- SqlPersistT ~ ReaderT SqlBackend
type Transaction m = SqlPersistT (ReaderT DataSource m)

data DataSourceConfig = DataSourceConfig
  { dstype  :: Text
  , dbname  :: Text
  , url     :: Text
  , user    :: Text
  , pass    :: Text
  , extra   :: Text
  , port    :: Int
  , thread  :: Int
  , enabled :: Bool
  } deriving Show

instance FromJSON DataSourceConfig where
  parseJSON (Object v) = DataSourceConfig
    <$> v .:? "type"      .!= "sqlite"
    <*> v .:? "dbname"    .!= ":memory:"
    <*> v .:? "url"       .!= "localhost"
    <*> v .:? "username"  .!= "sa"
    <*> v .:? "password"  .!= ""
    <*> v .:? "extra"     .!= ""
    <*> v .:? "port"      .!= 0
    <*> v .:? "pool-size" .!= 10
    <*> v .:? "enabled"   .!= True
  parseJSON v = typeMismatch "DataSourceConfig" v

instance Default DataSourceConfig where
  def = fromJust $ decode "{}"

data DataSourceProvider = DataSourceProvider
  { datasource           :: Text
  , currentSQL           :: Text
  , createConnectionPool :: DataSourceConfig -> LoggingT IO ConnectionPool
  }

newtype DataSource = DataSource (DataSourceProvider, DataSourceConfig , ConnectionPool)

instance Show DataSource where
  show (DataSource (_,dsc,_)) = show dsc

dataSource :: LoggerConfig -> DataSourceConfig -> [DataSourceProvider] -> IO DataSource
dataSource lc dsc@DataSourceConfig{..} ps = do
  logger lc INFO $ "Initialize database " <> toLogStr dstype <> "\n"
  case Prelude.lookup dstype $ fmap (\p->(datasource p,p)) ps of
    Nothing -> error $ "DataSource Type " <> unpack dstype <> " Not Supported"
    Just v  -> (\d -> DataSource (v,dsc,d)) <$> runLoggingT (createConnectionPool v dsc) (fixLn $ toMonadLogger lc)

closeDataSource :: LoggerConfig -> DataSource -> IO ()
closeDataSource lc (DataSource (_,DataSourceConfig{..},pool)) = do
  logger lc INFO $ "Close database " <> toLogStr dstype <> "\n"
  destroyAllResources pool

runTrans :: (LoggerMonad m, MonadUnliftIO m) => DataSource -> Transaction m a -> m a
runTrans ds trans = flip runReaderT ds $ do
  DataSource (_,_,pool) <- ask
  lc <- lift loggerConfig
  withRunInIO $ \run -> withResource pool $ run . \c -> runSqlConn trans c {connLogFunc = fixLn $ toMonadLogger lc}

fixLn :: LogFunc -> LogFunc
fixLn f a b c str = f a b c $ str <> "\n"

class FromPersistValue a where
  parsePersistValue :: [PersistValue] -> a

instance PersistField a => FromPersistValue [a] where
  parsePersistValue = rights . map fromPersistValue

instance FromPersistValue Text where
  parsePersistValue = intercalate "," . rights . map fromPersistValueText

query :: (MonadUnliftIO m, FromPersistValue a) => Text -> [PersistValue] -> Transaction m [a]
query sql params = do res <- rawQueryRes sql params
                      withAcquire res (\a -> runConduit $ a .| CL.fold i [])
  where i b ps = parsePersistValue ps : b

selectNow :: MonadUnliftIO m => Transaction m UTCTime
selectNow = do
  DataSource (p,_,_) <- lift ask
  head <$> selectValue (currentSQL p)

selectValue :: (PersistField a, MonadUnliftIO m) => Text -> Transaction m [a]
selectValue sql = fmap unSingle <$> rawSql sql []

instance LoggerMonad m => LoggerMonad (Transaction m) where
  loggerConfig = lift  $ lift loggerConfig
