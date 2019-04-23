{-# LANGUAGE DeriveAnyClass #-}
module Yam.Server.Health where

import           Data.Aeson
import qualified Data.HashMap.Strict as M
import           Data.Swagger
import           GHC.Generics
import           GHC.TypeLits
import           Servant
import           Yam.App
import           Yam.Logger
import           Yam.Prelude

type HealthEndpoint = "health" :> Get '[JSON] HealthResult

data HealthStatus = UP | DOWN deriving (Eq, Show, Generic, ToJSON)

instance ToSchema HealthStatus
instance ToSchema HealthResult

data HealthResult = HealthResult
  { status  :: HealthStatus
  , details :: M.HashMap Text HealthResult
  } deriving (Eq, Show, Generic, ToJSON)

emptyHealth :: HealthResult
emptyHealth = HealthResult UP M.empty

healthEndpoint :: (HasLogger cxt, MonadIO m) => Bool -> AppT cxt m HealthResult
healthEndpoint True = return emptyHealth
healthEndpoint _    = return emptyHealth
