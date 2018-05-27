module Yam.Transaction(
    Transaction
  , DataSourceConfig(..)
  , DataSourceProvider(..)
  , dataSource
  , DataSource
  , runTrans
  , query
  , selectValue
  , selectNow
  , now
  ) where

import           Yam.Logger

import           Control.Monad.IO.Unlift    (MonadUnliftIO)
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
import           Data.Text                  (Text, intercalate, unpack)
import           Data.Time                  (UTCTime)
import           Database.Persist.Sql

-- SqlPersistT ~ ReaderT SqlBackend
type Transaction m = SqlPersistT (ReaderT DataSource m)

data DataSourceConfig = DataSourceConfig
  { dstype :: Text
  , url    :: Text
  , user   :: Text
  , pass   :: Text
  , port   :: Int
  , thread :: Int
  } deriving Show

instance FromJSON DataSourceConfig where
  parseJSON (Object v) = DataSourceConfig
    <$> v .:? "type"      .!= "sqlite"
    <*> v .:? "url"       .!= ":memory:"
    <*> v .:? "username"  .!= "sa"
    <*> v .:? "password"  .!= ""
    <*> v .:? "port"      .!= 0
    <*> v .:? "pool-size" .!= 10
  parseJSON v = typeMismatch "DataSourceConfig" v

instance Default DataSourceConfig where
  def = fromJust $ decode "{}"

data DataSourceProvider = DataSourceProvider
  { datasource           :: Text
  , currentSQL           :: Text
  , createConnectionPool :: DataSourceConfig -> LoggingT IO ConnectionPool
  }

type DataSource = (DataSourceProvider, ConnectionPool)

dataSource :: LoggerConfig -> DataSourceConfig -> [DataSourceProvider] -> IO DataSource
dataSource lc dsc@DataSourceConfig{..} ps = case lookup dstype $ fmap (\p->(datasource p,p)) ps of
  Nothing -> error $ "DataSource Type " <> unpack dstype <> " Not Supported"
  Just v  -> (v,) <$> runLoggingT (createConnectionPool v dsc) (toMonadLogger lc)

runTrans :: MonadUnliftIO m => DataSource -> Transaction m a -> m a
runTrans ds trans = flip runReaderT ds $ do
  (_,pool) <- ask
  runSqlPool trans pool

class FromPersistValue a where
  parsePersistValue :: [PersistValue] -> a

instance PersistField a => FromPersistValue [a] where
  parsePersistValue = rights . map fromPersistValue

instance FromPersistValue Text where
  parsePersistValue = intercalate "," . rights . map fromPersistValueText

query :: FromPersistValue a => Text -> [PersistValue] -> Transaction IO [a]
query sql params = do res <- rawQueryRes sql params
                      withAcquire res (\a -> runConduit $ a .| CL.fold i [])
  where i b ps = parsePersistValue ps : b

selectNow :: Transaction IO UTCTime
selectNow = do
  (p,_) <- lift ask
  head <$> selectValue (currentSQL p)

selectValue :: (PersistField a) => Text -> Transaction IO [a]
selectValue sql = fmap unSingle <$> rawSql sql []

now :: DataSource -> IO UTCTime
now p = runTrans p selectNow




