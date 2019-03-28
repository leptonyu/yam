module Yam.Middleware.Client(
  -- * Client Middleware
    ClientConfig(..)
  , runClient
  , clientMiddleware
  , parseBaseUrl
  , BaseUrl
  ) where

import qualified Data.Vault.Lazy     as L
import           Network.HTTP.Client
import           Salak
import           Servant.Client
import           System.IO.Unsafe    (unsafePerformIO)
import           Yam.Middleware
import           Yam.Types

data ClientConfig = ClientConfig
  { enabled :: Bool
  } deriving (Eq, Show)

instance Default ClientConfig where
  def = ClientConfig True

instance FromProp ClientConfig where
  fromProp = ClientConfig <$> "enabled" .?= enabled def

{-# NOINLINE managerKey #-}
managerKey :: L.Key Manager
managerKey = unsafePerformIO newKey

clientMiddleware :: ClientConfig -> AppMiddleware
clientMiddleware ClientConfig{..} = AppMiddleware $ \e f -> do
  mg <- liftIO $ newManager defaultManagerSettings { managerModifyRequest = go e}
  f (setAttr managerKey mg e, id)
  where
    go env req = do
      runApp env $ logDebug $ showText req
      return req

runClient :: ClientM a -> BaseUrl -> App a
runClient action url = do
  mg <- requireAttr managerKey
  c  <- liftIO $ runClientM action $ mkClientEnv mg url
  case c of
    Left  e -> throw e
    Right a -> return a
