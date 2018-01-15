{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Yam.Transaction(
    Transaction
  , TransactionPool(..)
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
import           Yam.Prop

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
  setConnectionPool :: TransactionPool -> Maybe TransactionPool -> m ()
  secondaryPool  :: m (Maybe TransactionPool)
  secondaryPool = return Nothing

type DataSourceConnector m a = LogFunc -> DataSource -> (TransactionPool -> m a) -> m a
type DataSourceProvider  m a = (Text, DataSourceConnector m a)

class HasDataSource ds where
  connector :: (MonadTransaction m, MonadThrow m) => Proxy ds -> DataSourceConnector m a

data DataSourceException = DataSourcePoolNotFound Text
                         | DataSourceNotSupported Text
                         | DataSourceConfigNotFound Text
                         deriving Show

instance Exception DataSourceException

initDataSource :: (MonadTransaction m, MonadThrow m) => [DataSourceProvider m a] -> DataSource -> Maybe DataSource -> m a -> m a
initDataSource maps ds ds2nd action = let map = M.fromList maps in go map ds ds2nd action
  where go map ds ds2 action = do
          logger <- toMonadLogger
          getConnector map logger ds $ \p ->
            case ds2 of
              Nothing -> setConnectionPool p Nothing >> action
              Just s2 -> getConnector map logger s2 $ \v -> setConnectionPool p (Just v) >> action
        getConnector map logger ds = case M.lookup (dbtype ds) map of
            Nothing -> \_ -> throwM $ DataSourceNotSupported $ cs $ dbtype ds
            Just db -> db logger ds

runTrans :: MonadTransaction m => Transaction a -> m a
runTrans trans = connectionPool >>= executePool trans

executePool trans p = liftIO (runSqlPool trans p)

runSecondaryTrans :: (MonadTransaction m, MonadMask m) => Transaction a -> m a
runSecondaryTrans trans = do
  pool <- secondaryPool
  case pool of
    Nothing -> throwM $ DataSourcePoolNotFound "Secondary Pool"
    Just p  -> withLoggerName "Backup" $ executePool trans p

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
