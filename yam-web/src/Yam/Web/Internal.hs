{-# LANGUAGE NoPolyKinds #-}

module Yam.Web.Internal(
    prepareMiddleware
  , errorMiddleware
  , API
  , API'
  , mkServe
  , mkServe'
  , AppM
  , App
  ) where

import           Yam.Web.Middleware

import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.Dynamic
import           Data.Foldable              (foldr')
import           Data.Vault.Lazy
import           Network.Wai
import           Servant

type API c api = HasServer api '[Vault, c]
type AppM = ReaderT (Vault, Dynamic)
type App  = AppM Handler

runAppM :: Vault -> Dynamic -> AppM m a -> m a
runAppM v c ama = runReaderT ama (v, c)

type API' api = (Proxy api, Server api)

mkServe' :: (Typeable c, API c api, API c api') => (API' api -> API' api') -> Vault -> Proxy c -> c -> [Middleware] -> Proxy api -> ServerT api App -> Application
mkServe' f vault pa ka middlewares proxy server =
  let cxt     = toDyn ka
      server' = hoistServerWithContext proxy (fromPa pa) (runAppM vault cxt :: App m -> Handler m) server
      (p,s)   = f (proxy, server')
      app     = serveWithContext p (vault :. ka :. EmptyContext) s
      m       = prepareMiddleware (return . union vault)
  in foldr' ($) app $ m:middlewares
  where
    fromPa :: Proxy a -> Proxy '[Vault, a]
    fromPa _ = Proxy

mkServe :: (Typeable a, API a api) => Vault -> Proxy a -> a -> [Middleware] -> Proxy api -> ServerT api App -> Application
mkServe = mkServe' id
