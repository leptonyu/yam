module Main where

import           Control.Monad.Logger.CallStack
import qualified Data.Text                      as T
import           Paths_example                  (version)
import           Salak.Yaml
import           Servant
import           Yam

type AppS = App (Vault ': Simple)

newtype User = User { user :: T.Text } deriving (Eq, Show)

type UserApi
     = "users"    :> "error"   :> Get '[JSON] T.Text
  :<|> "users"    :> "servant" :> Get '[JSON] T.Text

service :: ServerT UserApi AppS
service = errorService :<|> servantService

errorService :: AppS T.Text
errorService = logError "No" >> return "No"

servantService :: AppS T.Text
servantService = throwS err401 "Servant"

main :: IO ()
main = startSimple YAML version (Proxy :: Proxy UserApi) service
