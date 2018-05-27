module Yam.Transaction.Postgresql where

import           Yam.Transaction

import           Data.Monoid                 ((<>))
import           Data.String.Conversions     (cs)
import           Data.Text                   (pack)
import           Database.Persist.Postgresql


postgresqlProvider :: DataSourceProvider
postgresqlProvider = DataSourceProvider "postgresql" "SELECT CURRENT_TIMESTAMP" go
  where
    go DataSourceConfig{..} =
      let port' = if port==0 then "5432" else pack (show port)
          conn  = "dbname=" <> dbname
               <> " host="  <> url
               <> " port="  <> port'
               <> " user="  <> user
               <> " password=" <> pass
               <> " " <> extra
      in createPostgresqlPool (cs conn) thread
