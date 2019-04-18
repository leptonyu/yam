module Yam.Config where

import           Salak
import           Yam.Prelude

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
