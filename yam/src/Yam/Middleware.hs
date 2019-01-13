{-# LANGUAGE ImplicitParams #-}
module Yam.Middleware(
    AppMiddleware(..)
  , simpleAppMiddleware
  , simpleWebMiddleware
  , runMiddleware
  ) where

import           Yam.Logger
import           Yam.Types

-- | Application Middleware
newtype AppMiddleware = AppMiddleware {runAM :: Env -> ((Env, Middleware)-> LoggingT IO ()) -> LoggingT IO ()}

instance Semigroup AppMiddleware where
  (AppMiddleware am) <> (AppMiddleware bm) = AppMiddleware $ \e f -> am e $ \(e', mw) -> bm e' $ \(e'',mw') -> f (e'', mw . mw')

instance Monoid AppMiddleware where
  mempty = AppMiddleware $ \a f -> f (a,id)

-- | Simple AppMiddleware
simpleAppMiddleware :: HasCallStack => (Bool, Text) -> Key a -> a -> AppMiddleware
simpleAppMiddleware (enabled,amname) k v =
  v `seq` if enabled
    then AppMiddleware $ \e f -> do
      logInfoCS ?callStack $ amname <> " enabled"
      f (setAttr k v e, id)
    else mempty

simpleWebMiddleware :: HasCallStack => (Bool, Text) -> Middleware -> AppMiddleware
simpleWebMiddleware (enabled,amname) m =
  if enabled
    then AppMiddleware $ \e f -> do
      logInfoCS ?callStack $ amname <> " enabled"
      f (e,m)
    else mempty

runMiddleware :: MonadIO m => AppMiddleware -> App a -> m ()
runMiddleware (AppMiddleware f) a = liftIO $ withLogger "test" def $ do
  lf <- askLoggerIO
  f (putLogger lf def) $ \(e,_) -> liftIO $ void $ runApp e a
