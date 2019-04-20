module Yam.Server(
    actuatorEndpoint
  , ActuatorEndpoint
  ) where

import           Salak
import           Servant
import           Yam.App
import           Yam.Logger
import           Yam.Server.Refresh

data ActuatorConfig = ActuatorConfig
  { enabled :: Bool
  , refresh :: Bool
  }

instance FromProp ActuatorConfig where
  fromProp = ActuatorConfig
    <$> "enabled"         .?= False
    <*> "refresh.enabled" .?= True

type ActuatorEndpoint = "actuator" :> RefreshEndpoint

endpoint :: HasLogger cxt => ActuatorConfig -> IO ReloadResult -> ServerT ActuatorEndpoint (AppV cxt IO)
endpoint ActuatorConfig{..} rr = (refreshEndpoint rr refresh)

actuatorEndpoint ::  HasLogger cxt => RunSalakT IO (Bool, ServerT ActuatorEndpoint (AppV cxt IO))
actuatorEndpoint = do
  ac@ActuatorConfig{..} <- require "actuator"
  (enabled,) <$> exec (\io -> return (endpoint ac io))
