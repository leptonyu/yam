module Main where

import           Control.Monad.Logger.CallStack
import qualified Data.Salak                     as S
import           Data.Time
import           Database.Persist.Sqlite
import           Servant
import           Yam

type UserApi
     = "users"              :> Get '[JSON] Text
  :<|> "users" :> "error"   :> Get '[JSON] Text
  :<|> "users" :> "servant" :> Get '[JSON] Text
  :<|> "users" :> "db"      :> Get '[JSON] Text

service :: ServerT UserApi App
service = userService :<|> errorService :<|> servantService :<|> dbService

userService :: App Text
userService = do
  logInfo $ "Hello: "
  return "Hello"

errorService :: App Text
errorService = logError "No" >> return "No"

servantService :: App Text
servantService = throwS err401 "Servant"

dbService :: App Text
dbService = do
  str <- runTrans $ do
    (time:_) :: [UTCTime] <- selectValue "SELECT CURRENT_TIMESTAMP"
    let timeStr = showText time
    logWarn timeStr
    return timeStr
  logInfo str
  return str

db :: DataSourceProvider
db = createSqlitePool ":memory:" 10

main :: IO ()
main = do
  p <- S.defaultPropertiesWithFile "yam_test.yml"
  start p [primaryDatasourceMiddleware db] (Proxy :: Proxy UserApi) service
