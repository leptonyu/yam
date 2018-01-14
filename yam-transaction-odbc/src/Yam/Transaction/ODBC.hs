{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Yam.Transaction.ODBC where

import           Yam.Import
import           Yam.Logger
import           Yam.Transaction

import qualified Data.Map              as M
import           Database.Persist.ODBC
import           GHC.TypeLits
import           System.Environment    (setEnv)

data ODBC (db :: Symbol)

instance KnownSymbol db => HasDataSource (ODBC db)  where
  connector p logger ds a = do
    let mayMap = do map <- extra ds
                    M.lookup "NLS_LANG" map
    case mayMap of
      Nothing -> throwM $ DataSourceConfigNotFound "extra.NLS_LANG"
      Just m  -> liftIO $ setEnv "NLS_LANG" $ cs m
    runLoggingT (withODBCPool (toDbTp $ symbolVal $ toP p) (cs $ conn ds) (thread ds) (lift.a)) logger
    where toP :: KnownSymbol db => Proxy (ODBC db) -> Proxy db
          toP _ = Proxy
          toDbTp :: String -> Maybe DBType
          toDbTp "oracle12c" = Just (Oracle True)
          toDbTp "oracle"    = Just (Oracle False)
          toDbTp _           = Nothing

oracleProvider :: MonadTransaction m => DataSourceProvider m a
oracleProvider    = ("oracle", connector (Proxy :: Proxy (ODBC "oracle")))
oracle12cProvider :: MonadTransaction m => DataSourceProvider m a
oracle12cProvider = ("oracle12c", connector (Proxy :: Proxy (ODBC "oracle12c")))
