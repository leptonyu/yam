module Main where

import           Control.Lens
import           Data.Pool
import           Salak
import           Salak.Yaml
import           Data.Swagger     hiding (version)
import qualified Data.Text        as T
import qualified Data.Vault.Lazy  as L
import           Paths_example    (version)
import           Servant
import           System.IO.Unsafe (unsafePerformIO)
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
  logWarn $ "Hello: " <> user token
  runPool (return ())
  return "Hello"

errorService :: App Text
errorService = logError "No" >> return "No"

servantService :: App Text
servantService = throwS err401 "Servant"

poolMW :: AppMiddleware
poolMW = simplePoolMiddleware (True, "test") poolKey open close
  where
    open = do
      lf <- askLoggerIO
      let l m = runLoggingT (logInfo m) lf
      liftIO $ createPool (l "start") (\_ -> l "end") 1 5 10
    close = liftIO . destroyAllResources

{-# NOINLINE poolKey #-}
poolKey :: L.Key (Pool ())
poolKey = unsafePerformIO newKey

runPool :: App () -> App ()
runPool a = do
  p <- requireAttr poolKey
  withRunInIO $ \f -> withResource p $ f . (\_ -> a)

main :: IO ()
main = runSalakWith "yam_test" YAML $ do
  al <- require  "yam.application"
  sw <- require  "yam.swagger"
  md <- require  "yam.middleware.default.enabled"
  lc <- requireD "yam.logging"
  exec $ \_ -> startYam al sw lc (fromMaybe True md) version [poolMW, authAppMiddleware checker] (Proxy :: Proxy UserApi) service
