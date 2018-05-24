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
import           Data.Foldable              (foldr')
import           Data.Vault.Lazy
import           Network.Wai
import           Servant

type API api = HasServer api '[Vault]
type AppM = ReaderT Vault
type App  = AppM Handler

runAppM :: Vault -> AppM m a -> m a
runAppM = flip runReaderT

type API' api = (Proxy api, Server api)

mkServe' :: (API api, API api') => (API' api -> API' api') ->  Vault -> [Middleware] -> Proxy api -> ServerT api App -> Application
mkServe' f vault middlewares proxy server =
  let server' = hoistServerWithContext proxy (Proxy :: Proxy '[Vault]) (runAppM vault :: App a -> Handler a) server
      (p,s)   = f (proxy, server')
      app     = serveWithContext p (vault :. EmptyContext) s
      m       = prepareMiddleware (return . union vault)
  in foldr' ($) app $ m:servantErrorMiddleware:middlewares

mkServe :: API api => Vault -> [Middleware] -> Proxy api -> ServerT api App -> Application
mkServe = mkServe' id
