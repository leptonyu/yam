module Yam.Config(
    AppConfig(..)
  , module Network.Wai.Handler.Warp
  ) where

import           Network.Wai.Handler.Warp
import           Salak
import           Yam.Prelude

-- | Application Configuration.
data AppConfig = AppConfig
  { name          :: Text   -- ^ Application name.
  , hostname      :: String -- ^ Applicatoin hostname, used in swagger.
  , port          :: Int    -- ^ Application http port.
  , slowlorisSize :: Int    -- ^ Slowloris size in Bytes, show in 'Settings'
  } deriving (Eq, Show)

instance Default AppConfig where
  def = AppConfig "application" "localhost" 8888 2048

instance FromProp AppConfig where
  fromProp = AppConfig
    <$> "name"           .?: name
    <*> "host"           .?: hostname
    <*> "port"           .?: port
    <*> "slowloris-size" .?: slowlorisSize
