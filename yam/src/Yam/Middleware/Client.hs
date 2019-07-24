{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Yam.Middleware.Client(
    HttpClient
  , HasHttpClient
  , runClient
  , hoistC
  , clientMW
  ) where

import           Data.Default
import           Data.Proxy
import           Network.HTTP.Client hiding (Proxy)
import           Salak
import           Servant.Client
import           Yam.Internal
import           Yam.Middleware

instance Default ManagerSettings where
  def = defaultManagerSettings

instance Monad m => FromProp m ResponseTimeout where
  fromProp = responseTimeoutMicro <$> fromProp

instance Monad m => FromProp m ManagerSettings where
  fromProp = do
    connCount <- "max-conns"  .?: managerConnCount
    timeout   <- "timeout"    .?: managerResponseTimeout
    idleCount <- "idle-conns" .?: managerIdleConnectionCount
    return def
      { managerConnCount           = connCount
      , managerResponseTimeout     = timeout
      , managerIdleConnectionCount = idleCount
      }

newtype HttpClient = HttpClient Manager

type HasHttpClient cxt = (HasBase cxt, HasCxt cxt HttpClient)

runClient :: HasHttpClient cxt => Proxy cxt -> BaseUrl -> ClientM a -> AppT cxt IO a
runClient _ url cma = do
  HttpClient m <- askCxt
  v <- liftIO $ runClientM cma (mkClientEnv m url)
  case v of
    Left  e -> throwM e
    Right r -> return r

hoistC :: forall cxt api. (HasHttpClient cxt, HasClient ClientM api) => Proxy cxt -> Proxy api -> BaseUrl -> Client (AppT cxt IO) api
hoistC pc p url = hoistClient p (runClient pc url) (client p)

clientMW :: HasBase cxt => (ManagerSettings -> ManagerSettings) -> AppMiddleware IO amtdcxt cxt HttpClient
clientMW fms = AppMiddleware $ \f -> do
    c  <- require "client"
    liftIO (HttpClient <$> newManager (fms c)) >>= f
