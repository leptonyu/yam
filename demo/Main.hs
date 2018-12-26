{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import qualified Data.Salak                           as S
import           Data.Text                            (Text)
import           Data.Time
import           Database.Persist.Sqlite
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           Yam

type UserApi
     = "users" :> Get '[JSON] Text
  :<|> "users" :> "error" :> Get '[JSON] Text
  :<|> "users" :> "servant" :> Get '[JSON] Text
  :<|> "users" :> "db" :> Get '[JSON] Text

userService :: App Text
userService = do
  (r,_) <- ask
  logInfo $ "Hello: " <> showText r
  return "Hello"

errorService :: App Text
errorService = logError "No" >> return "No"

servantService :: App Text
servantService = throwServant err401

dbService :: App Text
dbService = do
  str <- runDb $ do
    (time:_) :: [UTCTime] <- selectValue "SELECT CURRENT_TIMESTAMP"
    let timeStr = showText time
    logWarn timeStr
    return timeStr
  logInfo str
  return str

db :: DataSourceProvider
db DataSourceConfig{..} = createSqlitePool url maxThreads

main :: IO ()
main = do
  p <- S.defaultPropertiesWithFile "yam_test.yml"
  let config :: Maybe YamConfig = S.lookup "yam" p
  runStdoutLoggingT $ case config of
    Just c  -> start c (Proxy :: Proxy UserApi) (userService :<|> errorService :<|> servantService :<|> dbService) [logStdoutDev] (Just db) (return ())
    Nothing -> logError "Yam Config not found"
