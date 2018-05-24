module Yam.Transaction.Sqlite where

import           Yam.Transaction

import           Database.Persist.Sqlite

sqliteProvider :: DataSourceProvider
sqliteProvider = DataSourceProvider "sqlite" "SELECT CURRENT_TIMESTAMP" (\DataSourceConfig{..} -> createSqlitePool url thread)
