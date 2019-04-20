module Yam.Middleware where

import qualified Control.Category               as C
import           Control.Monad.Logger.CallStack
import           Data.Text                      (Text)
import           Network.Wai
import           Salak
import           Servant
import           Yam.App

-- | Application Middleware.
newtype AppMiddleware a b = AppMiddleware
  { runAM :: Context a -> Middleware -> (Context b -> Middleware -> LoggingT IO ()) -> LoggingT IO () }

instance C.Category AppMiddleware where
  id = AppMiddleware $ \a m f -> f a m
  (AppMiddleware fbc) . (AppMiddleware fab) = AppMiddleware $ \a m f -> fab a m $ \b m1 -> fbc b m1 f

-- | Simple Application Middleware, just provide a config to context.
simpleContext :: a -> AppMiddleware cxt (a ': cxt)
simpleContext a = AppMiddleware $ \c m f -> f (a :. c) m

-- | Simple Application Middleware, just provide a config to context.
simpleConfig' :: (HasSalak cxt, FromProp a) => Text -> (a -> AppT cxt (LoggingT IO) b) -> AppMiddleware cxt (b ': cxt)
simpleConfig' key g = AppMiddleware $ \c m f -> runAppT c (require key) >>= \a -> runAppT c (g a) >>= \b -> f (b :. c) m

-- | Simple Application Middleware, just provide a config to context.
simpleConfig :: (HasSalak cxt, FromProp a) => Text -> AppMiddleware cxt (a ': cxt)
simpleConfig key = simpleConfig' key return

-- | Simple Application Middleware, promote a 'Middleware' to 'AppMiddleware'
simpleMiddleware :: Middleware -> AppMiddleware cxt cxt
simpleMiddleware m = AppMiddleware $ \c m2 f -> f c (m . m2)
