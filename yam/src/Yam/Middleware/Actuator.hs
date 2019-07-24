module Yam.Middleware.Actuator(
    ActuatorConfig(..)
  , actuatorMiddleware
  ) where

import           Control.Monad.State
import           Salak
import           Servant
import           Yam.Actuator.Health
import           Yam.Actuator.Refresh
import           Yam.Internal
import           Yam.Middleware
import           Yam.Middleware.Swagger

data ActuatorConfig = ActuatorConfig
  { enabled :: Bool
  , refresh :: Bool
  , health  :: Bool
  }

instance Monad m => FromProp m ActuatorConfig where
  fromProp = ActuatorConfig
    <$> "enabled"         .?= False
    <*> "refresh.enabled" .?= True
    <*> "health.enabled"  .?= True

type ActuatorEndpoint = SwaggerTag "actuator" "Actuator API" :> "actuator" :>
  (    RefreshEndpoint
  :<|> HealthEndpoint
  )

actuatorMiddleware
  :: forall cxt amtdcxt m
  . (Monad m, HasBase cxt, HasServer ActuatorEndpoint amtdcxt, HasBase (Context amtdcxt))
  => AppMiddleware m amtdcxt cxt ()
actuatorMiddleware = do
  ActuatorConfig{..} <- require "actuator"
  AMTD{..} <- get
  let actuatorEndpoint = refreshEndpoint refresh :<|> healthEndpoint healthCheck health
  modifyServer enabled (Proxy @ActuatorEndpoint) actuatorEndpoint

