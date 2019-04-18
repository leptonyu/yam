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

type Simple = '[ LogFunc ]

startSimple
  :: forall file api. (HasLoad file, HasSwagger api , HasServer api Simple)
  => file
  -> Version
  -> Proxy api
  -> ServerT api (App (Vault ': Simple))
  -> IO ()
startSimple f v p a = runSalakWith "yam_test" f $ do
  al <- require  "yam.application"
  sw <- require  "yam.swagger"
  lc <- requireD "yam.logging"
  liftIO $ start al sw v lc (\_ -> return ()) C.id p a