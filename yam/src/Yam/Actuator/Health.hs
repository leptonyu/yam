{-# LANGUAGE DeriveAnyClass #-}
module Yam.Actuator.Health where

import           Control.Concurrent.MVar
import           Control.Exception       (catch)
import           Control.Monad           (join)
import           Data.Aeson
import qualified Data.HashMap.Strict     as M
import           Data.Swagger
import           GHC.Generics
import           Servant
import           Yam.Internal

type HealthEndpoint = "health" :> Get '[JSON] HealthResult

data HealthStatus = UP | DOWN deriving (Eq, Show, Generic, ToJSON)

instance ToSchema HealthStatus
instance ToSchema HealthResult

data HealthResult = HealthResult
  { status  :: HealthStatus
  , errMsg  :: Maybe String
  , details :: M.HashMap Text HealthResult
  } deriving (Eq, Show, Generic, ToJSON)

emptyHealth :: IO HealthResult
emptyHealth = return (HealthResult UP Nothing M.empty)

mergeHealth :: IO HealthStatus -> Text -> IO HealthResult -> IO HealthResult
mergeHealth ios na ior = do
  (err,s)          <- ((Nothing,) <$> ios) `catch` (\(e :: SomeException) -> return (Just (show e), DOWN))
  HealthResult{..} <- ior
  return (HealthResult (if s == DOWN then s else status) Nothing $ M.insert na (HealthResult s err M.empty) details)

healthEndpoint :: MonadIO m => MVar (IO HealthResult) -> Bool -> AppT cxt m HealthResult
healthEndpoint a True = liftIO $ join $ readMVar a
healthEndpoint a _    = liftIO $ join $ readMVar a
