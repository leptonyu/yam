{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Yam.Middleware.Client(
    HttpClient
  , HasHttpClient
  , runClient
  , hoistC
  , clientMiddleware
  , clientMiddleware'
  ) where

import           Data.Default
import           Data.Proxy
import           Network.HTTP.Client hiding (Proxy)
import           Salak
import           Servant
import           Servant.Client
import           Yam.App
import           Yam.Logger
import           Yam.Middleware
import           Yam.Prelude

instance Default ManagerSettings where
  def = defaultManagerSettings

instance FromProp ResponseTimeout where
  fromProp = responseTimeoutMicro <$> fromProp

instance FromProp ManagerSettings where
  fromProp = do
    connCount <- "max-conns"  .?: managerConnCount
    timeout   <- "timeout"    .?: managerResponseTimeout
    idleCount <- "idle-conns" .?: managerIdleConnectionCount
    return def
      { managerConnCount           = connCount
      , managerResponseTimeout     = timeout
      , managerIdleConnectionCount = idleCount
      }

data HttpClient = HttpClient Manager

type HasHttpClient cxt = (HasLogger cxt, HasContextEntry cxt HttpClient)

runClient :: HasHttpClient cxt => Proxy cxt -> BaseUrl -> ClientM a -> AppT cxt IO a
runClient _ url cma = do
  HttpClient m <- getEntry
  v <- liftIO $ runClientM cma (mkClientEnv m url)
  case v of
    Left  e -> throwS err400 $ showText e
    Right r -> return r

hoistC :: forall cxt api. (HasHttpClient cxt, HasClient ClientM api) => Proxy cxt -> Proxy api -> BaseUrl -> Client (AppT cxt IO) api
hoistC pc p url = hoistClient p (runClient pc url) (client p)

clientMiddleware :: RunSalak (AppMiddleware cxt (HttpClient : cxt))
clientMiddleware = clientMiddleware' id

clientMiddleware' :: (ManagerSettings -> ManagerSettings) -> RunSalak (AppMiddleware cxt (HttpClient : cxt))
clientMiddleware' f = do
  ms <- require "client" >>= liftIO . newManager . f
  return $ simpleContext $ HttpClient ms
