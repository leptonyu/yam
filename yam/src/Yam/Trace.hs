module Yam.Trace where

import           Data.Opentracing
import qualified Data.Vault.Lazy  as L
import           Network.Wai
import           System.IO.Unsafe (unsafePerformIO)
import           Yam.Logger
import           Yam.Types


{-# NOINLINE spanContextKey #-}
spanContextKey :: Key SpanContext
spanContextKey = unsafePerformIO newKey

{-# NOINLINE spanKey #-}
spanKey :: Key Span
spanKey = unsafePerformIO newKey

instance MonadIO m => MonadTracer (AppM m) where
  askSpanContext = requireAttr spanContextKey

instance MonadIO m => MonadTracing (AppM m) where
  runInSpan name notify action = do
    s <- askAttr spanKey
    n <- case s of
      Just sp -> newChildSpan name sp
      _       -> newSpan name
    notify n
    a <- withAttr spanKey n $ action n
    finishSpan n >>= notify
    return a

traceMw :: Env -> (Span -> App ()) -> Middleware
traceMw env notify app req resH = runAppM env $ runInSpan (fromMaybe "" $ listToMaybe $ pathInfo req) notify $ \s@Span{..} -> do
  let SpanContext{..} = context
      tid = traceId <> "/" <> spanId
      h   = ("X-TRACE-ID",encodeUtf8 tid)
      v   = L.insert extensionLogKey tid (vault req)
      v'  = L.insert spanKey s v
  liftIO $ app req {vault = v'} (resH . mapResponseHeaders (h:))

traceMiddleware :: Bool -> (Span -> App ()) -> AppMiddleware
traceMiddleware enabled notify
  = AppMiddleware $ \env f -> if enabled
    then do
      c <- newContext
      let env' = setAttr spanContextKey c env
      f (env', traceMw env' notify)
    else f (env, id)
