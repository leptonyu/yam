module Main where

import           Model

import           Yam.Logger
import           Yam.Transaction
import           Yam.Web

import           Control.Exception (throw)
import           Data.Text         (Text, pack)
import           Servant

type UserApi
     = "users" :> Get '[JSON] Text
  :<|> "users" :> "error" :> Get '[JSON] Text
  :<|> "users" :> "servant" :> Get '[JSON] Text
  :<|> "users" :> "db" :> Get '[JSON] Text

userService :: Yam Text
userService = do
  (r,_) <- ask
  errorLn $ "Hello: " <> toLogStr (show r)
  return "Hello"

errorService :: Yam Text
errorService = error "No"

servantService :: Yam Text
servantService = throw err401

dbService :: Yam Text
dbService = do
  str <- runDb $ do
    time <- selectNow
    let timeStr = pack $ show time
    warnLn timeStr
    return timeStr
  infoLn str
  return str

main :: IO ()
main = do
  ys <- defaultYamSettings
  logger (loggers ys) INFO (toLogStr (show ys) <> "\n")
  runDbIO ys migrateSql
  runServer ys (Proxy :: Proxy UserApi) (userService :<|> errorService :<|> servantService :<|> dbService)
