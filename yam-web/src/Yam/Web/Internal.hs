{-# LANGUAGE NoPolyKinds #-}

module Yam.Web.Internal(
    mkServe
  , mkServe'
  , ask
  , AppM
  , App
  , ReqApp
  ) where

import           Control.Monad.Trans.Class                  (lift)
import           Control.Monad.Trans.Reader
import           Data.Foldable                              (foldr')
import           Network.Wai
import           Servant
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication

type AppM c = ReaderT (Request, c)
type App  c = AppM c Handler
type ReqApp = ReaderT Request Handler

serveWithContext' :: HasServer api context => Proxy api -> Proxy context -> Context context -> (Request -> ServerT api ReqApp) -> Application
serveWithContext' p pc context server = toApplication (runRouter (route p context (go (go2 p pc) $ emptyDelayed (Route server))))
  where
    go :: (Request -> a -> b) -> Delayed env a -> Delayed env b
    go f Delayed{..} = Delayed
      { serverD = \ c p' h a b req -> f req <$> serverD c p' h a b req
      , ..
      }
    go2 :: HasServer api context => Proxy api -> Proxy context -> Request -> (Request -> ServerT api ReqApp) -> Server api
    go2 p2 pc2 req sar = downR p2 pc2 req (sar req)

mkServe' :: (HasServer api context, HasServer api' context)
         => (Server api -> Server api')
         -> Proxy api'
         -> Proxy api
         -> Proxy context
         -> Context context
         -> Proxy c
         -> c
         -> [Middleware]
         -> ServerT api (App c)
         -> Application
mkServe' f p' proxy pcxt cxt pc c middlewares server =
  let server' = hoistServerWithContext proxy pcxt (tranR pc c) server
      s       = fix1 proxy p' pcxt f server'
      app     = serveWithContext' p' pcxt cxt s
  in foldr' ($) app middlewares

mkServe :: HasServer api context
        => Proxy api
        -> Proxy context
        -> Context context
        -> Proxy c
        -> c
        -> [Middleware]
        -> ServerT api (App c)
        -> Application
mkServe p = mkServe' id p p

tranR :: Proxy c -> c -> App c m -> ReqApp m
tranR _ c = withReaderT (,c)

fix1 :: (HasServer api context, HasServer api' context) => Proxy api -> Proxy api' -> Proxy context -> (Server api -> Server api') -> ServerT api ReqApp -> Request -> ServerT api' ReqApp
fix1 p p' pc f sar req = liftR p' pc $ f (downR p pc req sar)


downR :: HasServer api context => Proxy api -> Proxy context -> Request -> ServerT api ReqApp -> Server api
downR p2 pc2 req = hoistServerWithContext p2 pc2 (flip runReaderT req :: ReqApp m -> Handler m)

liftR :: HasServer api context => Proxy api -> Proxy context -> Server api -> ServerT api ReqApp
liftR p pc = hoistServerWithContext p pc go
  where
    go :: Handler m -> ReqApp m
    go = lift
