module Main where

import           Control.Monad.Logger.CallStack
import qualified Data.Salak                     as S
import           Network.HTTP.Types
import           Paths_yam                      (version)
import           Servant
import           Yam

type CheckToken = CheckAuth Text

instance HasAuthKey Text

checker :: AuthChecker Text
checker = AuthChecker $ \req ->
  case lookup hAuthorization $ requestHeaders req of
    Just auth -> return $ decodeUtf8 auth
    _         -> throwS err401 "Permission Denied"

type UserApi
     = CheckToken :> "users"   :> Get '[JSON] Text
  :<|> "users"    :> "error"   :> Get '[JSON] Text
  :<|> "users"    :> "servant" :> Get '[JSON] Text

service :: ServerT UserApi App
service = userService :<|> errorService :<|> servantService

userService :: Text -> App Text
userService token = do
  logInfo $ "Hello: " <> token
  return "Hello"

errorService :: App Text
errorService = logError "No" >> return "No"

servantService :: App Text
servantService = throwS err401 "Servant"


main :: IO ()
main = do
  p <- S.defaultPropertiesWithFile "yam_test.yml"
  start p version [authAppMiddleware checker] (Proxy :: Proxy UserApi) service
