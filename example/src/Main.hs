{-# LANGUAGE NoPolyKinds #-}
module Main where

import           Control.Monad.Logger.CallStack
import qualified Data.Text                      as T
import           Paths_example                  (version)
import           Salak.Yaml
import           Servant
import           Yam
import           Yam.Redis

type App = AppV (REDIS : Simple) IO

newtype User = User { user :: T.Text } deriving (Eq, Show)

type UserApi
     = "users"    :> "error"   :> Get '[JSON] T.Text
  :<|> "users"    :> "servant" :> Get '[JSON] T.Text

service :: ServerT UserApi App
service = errorService :<|> servantService

errorService :: App T.Text
errorService = logError "No" >> return "No"

servantService :: App T.Text
servantService = throwS err401 "Servant"

main :: IO ()
main = start "yam_test" YAML version redisMiddleware (Proxy @UserApi) service
