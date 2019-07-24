module Yam.Server(
    actuatorEndpoint
  , ActuatorEndpoint
  , ActuatorConfig(..)
  ) where

import           Salak
import           Servant
import           Yam.App
import           Yam.Logger
import           Yam.Server.Health
import           Yam.Server.Refresh
import           Yam.Swagger

data ActuatorConfig = ActuatorConfig
  { enabled :: Bool
  , refresh :: Bool
  , health  :: Bool
  }

instance MonadCatch m => FromProp m ActuatorConfig where
  fromProp = ActuatorConfig
    <$> "enabled"         .?= False
    <*> "refresh.enabled" .?= True
    <*> "health.enabled"  .?= True

type ActuatorEndpoint = SwaggerTag "actuator" "Actuator API" :> "actuator" :>
  (    RefreshEndpoint
  :<|> HealthEndpoint
  )

actuatorEndpoint ::  (HasSalaks cxt, HasLogger cxt) => IO HealthResult -> ActuatorConfig -> ServerT ActuatorEndpoint (AppV cxt IO)
actuatorEndpoint rr ActuatorConfig{..} = refreshEndpoint refresh :<|> healthEndpoint rr health
