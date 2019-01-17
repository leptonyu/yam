module Main where

import           Control.Lens
import           Data.Salak    (defaultPropertiesWithFile)
import           Data.Swagger  hiding (version)
import qualified Data.Text     as T
import           Paths_example (version)
import           Servant
import           Yam

newtype User = User { user :: Text } deriving (Eq, Show)

type CheckToken = CheckAuth User

instance HasAuthKey User where
  toLog = user

instance ToParamSchema User where
  toParamSchema _ = mempty
    & pattern ?~ "^" <> authHeader <> " [a-zA-Z]{3,16}$"

authHeader :: Text
authHeader = "YAM/USER"

checker :: AuthChecker User
checker = AuthChecker $ \req ->
  case lookup hAuthorization $ requestHeaders req of
    Just auth -> do
      let (h,t) = T.breakOn " " $ decodeUtf8 auth
      check h (T.strip t)
    _         -> throwS err401 "Permission Denied"
  where
    check :: Text -> Text -> App User
    check "YAM/USER" u = return $ User u
    check _ _          = throwS err401 "Permission Denied"

type UserApi
     = CheckToken :> "users"   :> Get '[JSON] Text
  :<|> "users"    :> "error"   :> Get '[JSON] Text
  :<|> "users"    :> "servant" :> Get '[JSON] Text

service :: ServerT UserApi App
service = userService :<|> errorService :<|> servantService

userService :: User -> App Text
userService token = do
  logInfo $ "Hello: " <> user token
  return "Hello"

errorService :: App Text
errorService = logError "No" >> return "No"

servantService :: App Text
servantService = throwS err401 "Servant"

main :: IO ()
main = do
  p <- defaultPropertiesWithFile "yam_test.yml"
  start p version [authAppMiddleware checker] (Proxy :: Proxy UserApi) service
