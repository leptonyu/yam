module Yam.Middleware.Client(
    ClientConfig(..)
  , runClient
  , clientMiddleware
  , parseBaseUrl
  , BaseUrl
  ) where

import           Network.HTTP.Client
import           Servant.Client
import           System.IO.Unsafe    (unsafePerformIO)
import           Yam.Middleware
import           Yam.Types

data ClientConfig = ClientConfig
  { enabled :: Bool
  } deriving (Eq, Show)

instance Default ClientConfig where
  def = ClientConfig True

instance FromProperties ClientConfig where
  fromProperties p = ClientConfig
    <$> p .?> "enabled" .?= enabled def

{-# NOINLINE managerKey #-}
managerKey :: Key Manager
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
