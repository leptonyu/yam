module Yam.Server(
    actuatorEndpoint
  , ActuatorEndpoint
  ) where

import           Salak
import           Servant
import           Yam.App
import           Yam.Logger
import           Yam.Prelude
import           Yam.Server.Health
import           Yam.Server.Refresh

data ActuatorConfig = ActuatorConfig
  { enabled :: Bool
  , refresh :: Bool
  , health  :: Bool
  }

instance FromProp ActuatorConfig where
  fromProp = ActuatorConfig
    <$> "enabled"         .?= False
    <*> "refresh.enabled" .?= True
    <*> "health.enabled"  .?= True

type ActuatorEndpoint = "actuator" :>
  (    RefreshEndpoint
  :<|> HealthEndpoint
  )

endpoint :: HasLogger cxt => ActuatorConfig -> IO ReloadResult -> ServerT ActuatorEndpoint (AppV cxt IO)
endpoint ActuatorConfig{..} rr = refreshEndpoint rr refresh :<|> healthEndpoint health

actuatorEndpoint ::  HasLogger cxt => RunSalak (Bool, ServerT ActuatorEndpoint (AppV cxt IO))
actuatorEndpoint = do
  ac@ActuatorConfig{..} <- require "actuator"
  (enabled,) <$> liftSalak (exec $ return . endpoint ac)
