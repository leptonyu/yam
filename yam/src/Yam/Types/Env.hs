module Yam.Types.Env(
    AppConfig(..)
  , Env(..)
  , getAttr
  , reqAttr
  , setAttr
  ) where

import qualified Data.Vault.Lazy   as L
import           Yam.Types.Prelude

data AppConfig = AppConfig
  { name :: Text
  , port :: Int
  } deriving (Eq, Show)

instance FromJSON AppConfig where
  parseJSON = withObject "AppConfig" $ \v -> AppConfig
    <$> v .:? "name" .!= "application"
    <*> v .:? "port" .!= 8888

instance Default AppConfig where
  def = defJson

data Env = Env
  { attributes    :: Vault
  , reqAttributes :: Maybe Vault
  , application   :: AppConfig
  }

instance Default Env where
  def = Env L.empty Nothing def

getAttr :: Key a -> Env -> Maybe a
getAttr k Env{..} = (reqAttributes >>= L.lookup k) <|> L.lookup k attributes

reqAttr :: Default a => Key a -> Env -> a
reqAttr k = fromMaybe def . getAttr k

setAttr :: Key a -> a -> Env -> Env
setAttr k v Env{..} = case reqAttributes of
  Just av -> Env attributes (Just $ L.insert k v av)     application
  _       -> Env (L.insert k v attributes) reqAttributes application
