{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoPolyKinds         #-}
-- |
-- Module:      Yam.Middleware
-- Copyright:   2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- This module defines a standard application middleware used when starting application by yam.
--
-- Middleware vendors can package components into a `AppMiddleware` instance.
--
module Yam.Middleware(
  -- * Application middleware
    AppMiddleware(..)
  , runAM
  , modifyServer
  , modifyServer'
  , modifyHealthCheck
  , modifyMiddleware
  -- * Middleware monad
  , AMTD(..)
  , emptyAMTD
  , AppMW
  , runAMT
  , runNT
  ) where

import qualified Control.Category               as C
import           Control.Concurrent.MVar
import           Control.Monad.Catch            hiding (Handler)
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Control.Monad.State
import           Network.Wai
import           Salak
import           Servant
import           Yam.Actuator.Health
import           Yam.Internal

-- | Components that can be enhanced by `AppMiddleware`.
data AMTD amtdcxt = AMTD
  { middleware  :: Middleware             -- ^ Wai application middleware.
  , healthCheck :: MVar (IO HealthResult) -- ^ Health Check
  , serveYam    :: forall api. HasServer api amtdcxt => Proxy api -> Context amtdcxt -> Server api -> Application
  }

-- | Empty `AMTD` with given `Middleware`
emptyAMTD :: Middleware -> IO (AMTD amtdcxt)
emptyAMTD md = do
  eh <- newMVar emptyHealth
  return $ AMTD md eh serveWithContext

-- | Monad used for generate component @a@ in context @cxt@.
newtype AppMW amtdcxt cxt m a = AppMW (StateT (AMTD amtdcxt) (AppT cxt m) a)
  deriving (Functor, Applicative, Monad, MonadState (AMTD amtdcxt), MonadReader cxt)

instance MonadTrans (AppMW amtdcxt cxt) where
  lift = AppMW . lift . lift

instance MonadIO m => MonadIO (AppMW amtdcxt cxt m) where
  liftIO = AppMW . liftIO

instance Monad m => MonadThrow (AppMW amtdcxt cxt m) where
  throwM = AppMW . throwM

instance Monad m => MonadCatch (AppMW amtdcxt cxt m) where
  catch ma f = do
    v <- try ma
    case v of
      Left  e -> f e
      Right x -> return x

instance (Monad m, HasCxt cxt SourcePack) => MonadSalak (AppMW amtdcxt cxt m) where
  askSalak = AppMW (lift askSalak)

instance (MonadIO m, HasLogger cxt) => MonadLogger (AppMW amtdcxt cxt m) where
  monadLoggerLog a b c d = AppMW $ lift $ monadLoggerLog a b c d

instance (MonadIO m, HasLogger cxt) => MonadLoggerIO (AppMW amtdcxt cxt m) where
  askLoggerIO = AppMW $ lift $ askLoggerIO

-- | Run monad `AppMW`, produce a `AMTD` and custom component @a@.
runAMT :: MonadThrow m => AMTD amtdcxt -> cxt -> AppMW amtdcxt cxt m a -> m (a, AMTD amtdcxt)
runAMT am cxt (AppMW ma) = runAppT cxt $ runStateT ma am

-- liftX :: MonadThrow m => AppMW a m b -> AppMW b m c -> AppMW a m c
-- liftX fab fbc = do
--   a    <- ask
--   amtd <- get
--   (b, amtd2) <- lift $ runAMT amtd  a fab
--   (c, amtd3) <- lift $ runAMT amtd2 b fbc
--   put amtd3
--   return c

-- | Application middleware that produce a component @a@, and run with it.
--
newtype AppMiddleware m amtdcxt cxt a = AppMiddleware { buildAM :: (a -> AppMW amtdcxt cxt m ()) -> AppMW amtdcxt cxt m () } deriving Functor

instance Applicative (AppMiddleware m amtdcxt cxt) where
  pure a = AppMiddleware $ \f -> f a
  (AppMiddleware fa) <*> (AppMiddleware fb) = AppMiddleware $ \f -> fa $ \g -> fb $ \a -> f (g a)

instance Monad (AppMiddleware m amtdcxt cxt) where
  return = pure
  (AppMiddleware fa) >>= mfa = AppMiddleware $ \f -> fa $ \a -> buildAM (mfa a) f

instance Monad m => MonadState (AMTD amtdcxt) (AppMiddleware m amtdcxt cxt) where
  state fs = AppMiddleware $ \f -> do
    amtd <- get
    let (a, amtd2) = fs amtd
    put amtd2
    f a

instance Monad m => MonadReader cxt (AppMiddleware m amtdcxt cxt) where
  ask = AppMiddleware $ \f -> ask >>= f
  local f (AppMiddleware fma) = AppMiddleware $ \fmb -> fma $ local f . fmb

instance Monad m => MonadThrow (AppMiddleware m amtdcxt cxt) where
  throwM e = AppMiddleware $ \f -> throwM e >>= f

instance Monad m => MonadCatch (AppMiddleware m amtdcxt cxt) where
  catch ma f = do
    v <- try ma
    case v of
      Left  e -> f e
      Right x -> return x


instance (MonadIO m, HasLogger cxt) => MonadLogger (AppMiddleware m amtdcxt cxt) where
  monadLoggerLog a b c d = AppMiddleware $ \f -> monadLoggerLog a b c d >>= f

instance (Monad m, HasCxt cxt SourcePack) => MonadSalak (AppMiddleware m amtdcxt cxt) where
  askSalak = AppMiddleware $ \f -> askSalak >>= f

liftAM :: Monad m => m a -> AppMiddleware m amtdcxt cxt a
liftAM ma = AppMiddleware $ \f -> lift ma >>= f

instance MonadIO m => MonadIO (AppMiddleware m amtdcxt cxt) where
  liftIO = liftAM . liftIO

-- | Run application middleware.
runAM :: (MonadIO m, MonadThrow m)
  => AppMiddleware m amtdcxt cxt a
  -> cxt
  -> AMTD amtdcxt
  -> (a -> AMTD amtdcxt -> m ())
  -> m ()
runAM (AppMiddleware f) cxt amtd fa = void $ runAMT amtd cxt $ f (\a -> get >>= lift . fa a)

instance (MonadIO m, MonadThrow m) => C.Category (AppMiddleware m amtdcxt) where
  id  = AppMiddleware (ask >>=)
  (.) fbc fab = AppMiddleware $ \f -> do
    a    <- ask
    amtd <- get
    lift
      $ runAM fab a amtd
      $ \b amtd2 -> runAM fbc b amtd2
      $ \c amtd3 -> void $ runAMT amtd3 a (f c)

-- | Add additional api to server.
modifyServer :: forall m api amtdcxt cxt.
  ( Monad m
  , HasServer api amtdcxt)
  => Bool -- ^ If add server
  -> Proxy api
  -> ServerT api (AppT (Context amtdcxt) IO)
  -> AppMiddleware m amtdcxt cxt ()
modifyServer enable api server = modify $ \amtd -> if enable
    then amtd { serveYam = \a c s -> serveYam amtd (proxyAdd a api) c (s :<|> hoistServerWithContext api (Proxy @amtdcxt) (runNT c) server) }
    else amtd

proxyAdd :: Proxy a -> Proxy b -> Proxy (a :<|> b)
proxyAdd _ _ = Proxy

-- | Add additional api to server.
modifyServer' :: forall m api amtdcxt cxt.
  ( Monad m
  , HasServer api amtdcxt)
  => Bool -- ^ If add server
  -> Proxy api
  -> ServerT api Handler
  -> AppMiddleware m amtdcxt cxt ()
modifyServer' enable api server = modify $ \amtd -> if enable
    then amtd { serveYam = \a c s -> serveYam amtd (proxyAdd a api) c (s :<|> server) }
    else amtd


runNT :: cxt -> AppT cxt IO a -> Handler a
runNT c ma = liftIO $ runAppT c ma

-- | Modify middleware.
modifyMiddleware :: Monad m => (Middleware -> Middleware) -> AppMiddleware m amtdcxt cxt ()
modifyMiddleware fm = modify $ \amtd -> amtd { middleware = fm $ middleware amtd }

-- | Add health checker.
modifyHealthCheck :: MonadIO m => (IO HealthResult -> IO HealthResult) -> AppMiddleware m amtdcxt cxt ()
modifyHealthCheck fm = do
  AMTD{..} <- get
  liftIO $ modifyMVar_ healthCheck $ \a -> return (fm a)





