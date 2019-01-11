{-# LANGUAGE ImplicitParams #-}
module Yam.Middleware(
    AppMiddleware(..)
  , simpleAppMiddleware
  , simpleWebMiddleware
  ) where

import           Yam.Types.Env
import           Yam.Types.Prelude

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
