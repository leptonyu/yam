module Yam.Types.Env(
  -- * Environment
    AppConfig(..)
  , Env(..)
  , getAttr
  , reqAttr
  , setAttr
  ) where

import           Salak
import qualified Data.Vault.Lazy   as L
import           Yam.Types.Prelude

data AppConfig = AppConfig
  { name          :: Text
  , port          :: Int
  , slowlorisSize :: Int -- Bytes
  } deriving (Eq, Show)

instance Default AppConfig where
  def = AppConfig "application" 8888 2048

instance FromProp AppConfig where
  fromProp = AppConfig
    <$> "name"           .?: name
    <*> "port"           .?: port
    <*> "slowloris-size" .?: slowlorisSize

data Env = Env
  { attributes    :: Vault
  , reqAttributes :: Maybe Vault
  , application   :: AppConfig
  }

instance Default Env where
  def = Env L.empty Nothing def

getAttr :: L.Key a -> Env -> Maybe a
getAttr k Env{..} = (reqAttributes >>= L.lookup k) <|> L.lookup k attributes

reqAttr :: Default a => L.Key a -> Env -> a
reqAttr k = fromMaybe def . getAttr k

setAttr :: L.Key a -> a -> Env -> Env
setAttr k v Env{..} = case reqAttributes of
  Just av -> Env attributes (Just $ L.insert k v av)     application
  _       -> Env (L.insert k v attributes) reqAttributes application
