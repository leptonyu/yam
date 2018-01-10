{-# LANGUAGE OverloadedStrings #-}

module Yam.Transaction.Postgresql where

import           Yam.Import
import           Yam.Logger.MonadLogger
import           Yam.Transaction

import           Database.Persist.Postgresql


data PostgreSQL

instance HasDataSource PostgreSQL where
  connector _ logger ds a = runLoggingT (withPostgresqlPool (cs $ conn ds) (thread ds) (lift.a)) logger


postgresqlProvider :: MonadTransaction m => DataSourceProvider m a
postgresqlProvider = ("postgresql", connector (Proxy :: Proxy PostgreSQL))
