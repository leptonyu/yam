{-# LANGUAGE CPP            #-}
{-# LANGUAGE ImplicitParams #-}
module Yam.Middleware(
    AppMiddleware(..)
  , simpleAppMiddleware
  , simpleWebMiddleware
  , simplePoolMiddleware
  , runMiddleware
  ) where

import           Yam.Logger
import           Yam.Types

-- | Application Middleware
newtype AppMiddleware = AppMiddleware {runAM :: Env -> ((Env, Middleware)-> LoggingT IO ()) -> LoggingT IO ()}

instance Monoid AppMiddleware where
  mempty = AppMiddleware $ \a f -> f (a,id)
#if __GLASGOW_HASKELL__ >= 804
instance Semigroup AppMiddleware where
  (<>) = _append
#else
  mappend = _append
#endif


_append (AppMiddleware am) (AppMiddleware bm) = AppMiddleware $ \e f -> am e $ \(e', mw) -> bm e' $ \(e'',mw') -> f (e'', mw . mw')

-- | Simple AppMiddleware
simpleAppMiddleware :: HasCallStack => (Bool, Text) -> Key a -> a -> AppMiddleware
simpleAppMiddleware (enabled,amname) k v =
  v `seq` if enabled
    then AppMiddleware $ \e f -> do
      logInfoCS ?callStack $ "app:" <> amname <> " enabled"
      f (setAttr k v e, id)
    else mempty

simpleWebMiddleware :: HasCallStack => (Bool, Text) -> Middleware -> AppMiddleware
simpleWebMiddleware (enabled,amname) m =
  if enabled
    then AppMiddleware $ \e f -> do
      logInfoCS ?callStack $ "web:" <> amname <> " enabled"
      f (e,m)
    else mempty

simplePoolMiddleware :: HasCallStack => (Bool, Text) -> Key a -> App a -> (a -> App ()) -> AppMiddleware
simplePoolMiddleware (enabled, amname) key open close =
  if enabled
    then AppMiddleware $ \e f -> do
      logInfoCS ?callStack $ "pool:" <> amname <> " enabled"
      lf <- askLoggerIO
      liftIO $ bracket (runApp e open) (runApp e . close) $ \a -> runLoggingT (f (setAttr key a e, id)) lf
    else mempty

runMiddleware :: MonadIO m => AppMiddleware -> App a -> m ()
runMiddleware (AppMiddleware f) a = liftIO $ withLogger "test" def $ do
  lf <- askLoggerIO
  f (putLogger lf def) $ \(e,_) -> liftIO $ void $ runApp e a
