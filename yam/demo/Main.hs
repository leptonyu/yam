module Main where

import           Control.Monad.Logger.CallStack
import qualified Data.Salak                     as S
import           Servant
import           Yam

type UserApi
     = "users"              :> Get '[JSON] Text
  :<|> "users" :> "error"   :> Get '[JSON] Text
  :<|> "users" :> "servant" :> Get '[JSON] Text

service :: ServerT UserApi App
service = userService :<|> errorService :<|> servantService

userService :: App Text
userService = do
  logInfo $ "Hello: "
  return "Hello"

errorService :: App Text
errorService = logError "No" >> return "No"

servantService :: App Text
servantService = throwS err401 "Servant"

main :: IO ()
main = do
  p <- S.defaultPropertiesWithFile "yam_test.yml"
  start p [] (Proxy :: Proxy UserApi) service
