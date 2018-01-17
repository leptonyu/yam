{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Yam.Transaction.Sqlite where

import           Control.Monad.Logger    (runLoggingT)
import           Yam.Import
import           Yam.Transaction

import           Database.Persist.Sqlite


data SQLite

instance HasDataSource SQLite where
  connector _ logger ds a = runLoggingT (withSqlitePool (conn ds) (toThread ds) (lift.a)) logger
    where toThread d | conn d == conn def = 1
                     | otherwise           = thread d

sqliteProvider :: (MonadTransaction m, MonadThrow m) => DataSourceProvider m a
sqliteProvider = ("sqlite", connector (Proxy :: Proxy SQLite))
