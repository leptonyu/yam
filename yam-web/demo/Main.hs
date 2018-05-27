module Main where

import           Yam.Logger
import           Yam.Web

import           Control.Exception (throw)
import           Data.Text         (Text)
import           Servant

type UserApi = "users" :> Get '[JSON] Text
  :<|> "users" :> "error" :> Get '[JSON] Text
  :<|> "users" :> "servant" :> Get '[JSON] Text

userService :: Yam Text
userService = do
  (r,_) <- ask
  errorLn $ "Hello: " <> toLogStr (show r)
  return "Hello"

errorService :: Yam Text
errorService = error "No"

servantService :: Yam Text
servantService = throw err401

main :: IO ()
main = do
  ys <- defaultYamSettings
  logger (loggers ys) INFO (toLogStr (show ys) <> "\n")
  runServer ys (Proxy :: Proxy UserApi) (userService :<|> errorService :<|> servantService)
