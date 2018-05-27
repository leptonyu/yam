{-# LANGUAGE UndecidableInstances #-}

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
  ) where

import           Yam.Logger

import           Control.Monad.IO.Class     (MonadIO)
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
import           Data.Pool                  (withResource)
import           Data.Text                  (Text, intercalate, unpack)
import           Data.Time                  (UTCTime)
import           Data.Vault.Lazy
import           Database.Persist.Sql

-- SqlPersistT ~ ReaderT SqlBackend
type Transaction m = SqlPersistT (ReaderT DataSource m)

data DataSourceConfig = DataSourceConfig
  { dstype  :: Text
  , url     :: Text
  , user    :: Text
  , pass    :: Text
  , port    :: Int
  , thread  :: Int
  , enabled :: Bool
  } deriving Show

instance FromJSON DataSourceConfig where
  parseJSON (Object v) = DataSourceConfig
    <$> v .:? "type"      .!= "sqlite"
    <*> v .:? "url"       .!= ":memory:"
    <*> v .:? "username"  .!= "sa"
    <*> v .:? "password"  .!= ""
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

newtype DataSource = DataSource (DataSourceProvider, DataSourceConfig , LoggerConfig, ConnectionPool)

instance Show DataSource where
  show (DataSource (_,dsc,_,_)) = show dsc

dataSource :: LoggerConfig -> DataSourceConfig -> [DataSourceProvider] -> IO DataSource
dataSource lc dsc@DataSourceConfig{..} ps = do
  logger lc INFO $ "Initialize database " <> toLogStr dstype <> "\n"
  case Prelude.lookup dstype $ fmap (\p->(datasource p,p)) ps of
    Nothing -> error $ "DataSource Type " <> unpack dstype <> " Not Supported"
    Just v  -> (\d -> DataSource (v,dsc,lc,d)) <$> runLoggingT (createConnectionPool v dsc) (toMonadLogger lc)

runTrans :: MonadUnliftIO m => Vault -> DataSource -> Transaction m a -> m a
runTrans vault ds trans = flip runReaderT ds $ do
  DataSource (_,_,lc,pool) <- ask
  withRunInIO $ \run -> withResource pool $ run . \c -> runSqlConn trans c {connLogFunc = toMonadLogger lc {logVault=vault}}

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
  DataSource (p,_,_,_) <- lift ask
  head <$> selectValue (currentSQL p)

selectValue :: (PersistField a) => Text -> Transaction IO [a]
selectValue sql = fmap unSingle <$> rawSql sql []

class MonadIO m => TransMonad m where
  transConfig :: m DataSource
