{-# LANGUAGE IncoherentInstances #-}
module Yam.Middleware where

import qualified Control.Category               as C
import           Control.Monad.Logger.CallStack
import           Data.Text                      (Text)
import           Network.Wai
import           Salak
import           Servant
import           Yam.App
import           Yam.Server.Health

-- | Application Middleware.
newtype AppMiddleware a b = AppMiddleware
  { runAM :: Context a -> Middleware -> IO HealthResult -> (Context b -> Middleware -> IO HealthResult -> LoggingT IO ()) -> LoggingT IO () }

instance C.Category AppMiddleware where
  id = AppMiddleware $ \a m h f -> f a m h
  (AppMiddleware fbc) . (AppMiddleware fab) = AppMiddleware $ \a m h f -> fab a m h $ \b m1 h1 -> fbc b m1 h1 f

-- | Simple Application Middleware, just provide a config to context.
simpleContext :: a -> AppMiddleware cxt (a ': cxt)
simpleContext a = AppMiddleware $ \c m h f -> f (a :. c) m h

-- | Simple Application Middleware, just provide a config to context.
simpleConfig' :: (HasSalaks cxt, FromProp (AppT cxt (LoggingT IO)) a) => Text -> (a -> AppT cxt (LoggingT IO) b) -> AppMiddleware cxt (b ': cxt)
simpleConfig' key g = AppMiddleware $ \c m h f -> runAppT c (require key) >>= \a -> runAppT c (g a) >>= \b -> f (b :. c) m h

-- | Simple Application Middleware, just provide a config to context.
simpleConfig :: (HasSalaks cxt, FromProp (AppT cxt (LoggingT IO)) a) => Text -> AppMiddleware cxt (a ': cxt)
simpleConfig key = simpleConfig' key return

-- | Simple Application Middleware, promote a 'Middleware' to 'AppMiddleware'
simpleMiddleware :: Middleware -> AppMiddleware cxt cxt
simpleMiddleware m = AppMiddleware $ \c m2 h f -> f c (m . m2) h
