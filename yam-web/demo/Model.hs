{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Model where

import           Yam.Transaction

import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           Database.Persist.Sql
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
EntityAlert json sql=t_alert
  key        Text
  result     Text
  notifyKey  Text Maybe
  alert      Bool
  createTime UTCTime
  deriving Show

EntityAlertNotification json sql=t_alert_notification
  notifyKey  Text
  key        Text
  alertId    EntityAlertId
  createTime UTCTime
  EntityAlertNotificationUniqueKey notifyKey key
|]

migrateSql :: MonadIO m => Transaction m ()
migrateSql = runMigration migrateAll
