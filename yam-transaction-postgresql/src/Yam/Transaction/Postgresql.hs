{-# LANGUAGE OverloadedStrings #-}

module Yam.Transaction.Postgresql where

import           Yam.Import
import           Yam.Logger
import           Yam.Transaction

import           Database.Persist.Postgresql


data PostgreSQL

instance HasDataSource PostgreSQL where
  connector _ lg ds a = runLoggingT (withPostgresqlPool (cs $ conn ds) (thread ds) (lift.a)) lg


postgresqlProvider :: (MonadTransaction m, MonadThrow m) => DataSourceProvider m a
postgresqlProvider = ("postgresql", connector (Proxy :: Proxy PostgreSQL))
