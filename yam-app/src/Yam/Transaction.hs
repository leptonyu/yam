{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Yam.Transaction(
    Transaction
  , TransactionPool
  , DataSource(..)
  , MonadTransaction(..)
  , DataSourceConnector
  , DataSourceProvider
  , HasDataSource(..)
  , DataSourceException(..)
  , runTrans
  , selectValue
  , selectNow
  , now
  , initDataSource
  , runSecondaryTrans
  , query
  ) where

import           Yam.Import
import           Yam.Logger

import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Acquire                (with)
import           Data.Aeson
import           Data.Conduit
import qualified Data.Conduit.List           as CL
import           Data.Either                 (rights)
import qualified Data.Map                    as M
import           Data.Pool
import qualified Data.Text                   as T
import           Database.Persist.Sql

-- SqlPersistT ~ ReaderT SqlBackend
type Transaction  = SqlPersistT IO

type TransactionPool = Pool SqlBackend

data DataSource = DataSource
  { dbtype  :: Text
  , conn    :: Text
  , thread  :: Int
  , migrate :: Bool
  , extra   :: Maybe (M.Map Text Text)
  } deriving Show

instance FromJSON DataSource where
  parseJSON (Object v) = do
    dsDt <- v .:? "type"    .!= dbtype def
    dsCn <- v .:? "conn"    .!= conn   def
    dsTh <- v .:? "thread"  .!= thread def
    dsMi <- v .:? "migrate" .!= Yam.Transaction.migrate def
    dsEx <- v .:? "extra"
    return $ DataSource dsDt dsCn dsTh dsMi dsEx
  parseJSON v = typeMismatch "DataSource" v

instance Default DataSource where
  def = DataSource "sqlite" ":memory:" 10 True Nothing

class (MonadIO m, MonadBaseControl IO m, MonadYamLogger m) => MonadTransaction m where
  connectionPool :: m TransactionPool
  withConnectionPool :: TransactionPool -> Maybe TransactionPool -> m a -> m a
  secondaryPool  :: m (Maybe TransactionPool)
  secondaryPool = return Nothing

type DataSourceConnector m a = LogFunc -> DataSource -> (TransactionPool -> m a) -> m a
type DataSourceProvider  m a = (Text, DataSourceConnector m a)

class HasDataSource ds where
  connector :: (MonadTransaction m, MonadUnliftIO m) => Proxy ds -> DataSourceConnector m a

data DataSourceException = DataSourcePoolNotFound Text
                         | DataSourceNotSupported Text
                         | DataSourceConfigNotFound Text
                         deriving Show

instance Exception DataSourceException

initDataSource :: (MonadTransaction m, MonadMask m) => [DataSourceProvider m a] -> DataSource -> Maybe DataSource -> m a -> m a
initDataSource maps ds1 ds2nd action = let m = M.fromList maps in go m ds1 ds2nd action
  where go m' ds ds2 action' = do
          lg <- toMonadLogger
          getConnector m' lg ds $ \p ->
            case ds2 of
              Nothing -> withConnectionPool p Nothing action'
              Just s2 -> getConnector m' lg s2 $ \v -> withConnectionPool p (Just v) action'
        getConnector m2 l ds = case M.lookup (dbtype ds) m2 of
            Nothing -> \_ -> throwM $ DataSourceNotSupported $ cs $ dbtype ds
            Just db -> db l ds

runTrans :: MonadTransaction m => Transaction a -> m a
runTrans trans = connectionPool >>= liftIO . runSqlPool trans

runSecondaryTrans :: (MonadTransaction m, MonadMask m) => Transaction a -> m a
runSecondaryTrans trans = do
  pool <- secondaryPool
  case pool of
    Nothing -> throwM $ DataSourcePoolNotFound "Secondary Pool"
    Just p  -> withLoggerName "Backup" $ liftIO $ runSqlPool trans p

class FromPersistValue a where
  parsePersistValue :: [PersistValue] -> a

instance PersistField a => FromPersistValue [a] where
  parsePersistValue = rights . map fromPersistValue

instance FromPersistValue Text where
  parsePersistValue = T.intercalate "," . rights . map fromPersistValueText

query :: FromPersistValue a => Text -> [PersistValue] -> Transaction [a]
query sql params = do res <- rawQueryRes sql params
                      liftIO $ with res ($$ CL.fold i [])
  where i b ps = parsePersistValue ps : b

selectNow :: Transaction UTCTime
selectNow = head <$> (ask >>= dbNow . connRDBMS)
  where dbNow :: Text -> Transaction [UTCTime]
        dbNow "sqlite"     = selectValue "SELECT CURRENT_TIMESTAMP"
        dbNow "postgresql" = selectValue "SELECT CURRENT_TIMESTAMP"
        dbNow "oracle"     = selectValue "SELECT SYSDATE FROM DUAL"
        dbNow dbms         = throwM $ DataSourceNotSupported $ cs dbms

selectValue :: (PersistField a) => Text -> Transaction [a]
selectValue sql = fmap unSingle <$> rawSql sql []

now :: MonadTransaction m => m UTCTime
now = runTrans selectNow
