module Yam.Web.Demo where

import           Yam.Logger
import           Yam.Web

import           Control.Exception (throw)
import           Data.Text         (Text, pack)
import           Servant

type UserApi = "users" :> Get '[JSON] Text
  :<|> "users" :> "error" :> Get '[JSON] Text
  :<|> "users" :> "servant" :> Get '[JSON] Text

userService :: App Text
userService = return "Hello"

errorService :: App Text
errorService = error "No"

servantService :: App Text
servantService = throw err401

runDemo :: IO ()
runDemo = do
  ys <- defaultYamSettings
  infoLn (loggers ys) (pack $ show ys)
  runServer ys (Proxy :: Proxy UserApi) (userService :<|> errorService :<|> servantService)
