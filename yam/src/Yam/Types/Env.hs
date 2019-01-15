module Yam.Types.Env(
  -- * Environment
    AppConfig(..)
  , Env(..)
  , getAttr
  , reqAttr
  , setAttr
  ) where

import qualified Data.Vault.Lazy   as L
import           Yam.Types.Prelude

data AppConfig = AppConfig
  { name          :: Text
  , port          :: Int
  , slowlorisSize :: Int -- Bytes
  } deriving (Eq, Show)

instance FromProperties AppConfig where
  fromProperties p = AppConfig
    <$> p .?> "name"           .?= name def
    <*> p .?> "port"           .?= port def
    <*> p .?> "slowloris-size" .?= slowlorisSize def

instance Default AppConfig where
  def = AppConfig "application" 8888 2048

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
