{-# LANGUAGE NoPolyKinds #-}
module Main where

import           Control.Monad.Logger.CallStack
import qualified Data.Text                      as T
import           Data.Version                   (Version)
import           Paths_example                  (version)
import           Salak
import           Salak.Yaml
import           Servant
import           Servant.Swagger
import           Yam

type App = AppV Simple IO

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
main = startSimple YAML version (Proxy :: Proxy UserApi) service

type Simple = '[ LogFuncHolder ]

startSimple
  :: forall file api. (HasLoad file, HasSwagger api , HasServer api Simple)
  => file
  -> Version
  -> Proxy api
  -> ServerT api (AppV Simple IO)
  -> IO ()
startSimple f v p a = runSalakWith "yam_test" f $ do
  al <- require  "yam.application"
  sw <- require  "yam.swagger"
  lc <- requireD "yam.logging"
  liftIO $ start al sw v lc spanNoNotifier emptyAM serveWarp p a
