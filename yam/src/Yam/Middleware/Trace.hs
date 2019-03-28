module Yam.Middleware.Trace(
  -- * Trace Middleware
    MonadTracer(..)
  , MonadTracing(..)
  , TraceConfig(..)
  , hTraceId
  , hParentTraceId
  , hSpanId
  , hSampled
  , traceMiddleware
  ) where

import qualified Data.HashMap.Lazy as HM
import           Data.Opentracing
import qualified Data.Text         as T
import qualified Data.Vault.Lazy   as L
import           Salak
import           System.IO.Unsafe  (unsafePerformIO)
import           Yam.Logger
import           Yam.Middleware
import           Yam.Types

data TraceConfig = TraceConfig
  { enabled :: Bool
  , method  :: TraceNotifyType
  } deriving (Eq, Show)

data TraceNotifyType
  = NoTracer
  deriving (Eq, Show)

instance FromEnumProp TraceNotifyType where
  fromEnumProp _ = Right NoTracer

instance FromProp TraceConfig where
  fromProp = TraceConfig
    <$> "enabled" .?= enabled def
    <*> "type"    .?= method  def

instance Default TraceConfig where
  def = TraceConfig True NoTracer

notifier :: TraceNotifyType -> Span -> App ()
notifier _ _ = return ()

{-# NOINLINE spanContextKey #-}
spanContextKey :: L.Key SpanContext
spanContextKey = unsafePerformIO newKey

{-# NOINLINE spanKey #-}
spanKey :: L.Key Span
spanKey = unsafePerformIO newKey

instance MonadTracer App where
  askSpanContext = requireAttr spanContextKey

instance MonadTracing App where
  runInSpan name notify action = do
    s <- askAttr spanKey
    n <- case s of
      Just sp -> newChildSpan name sp
      _       -> newSpan name
    notify n
    a <- withAttr spanKey n $ action n
    finishSpan n >>= notify
    return a

hTraceId :: HeaderName
hTraceId = "X-B3-TraceId"

hParentTraceId :: HeaderName
hParentTraceId = "X-B3-ParentSpanId"

hSpanId :: HeaderName
hSpanId = "X-B3-SpanId"

hSampled :: HeaderName
hSampled = "X-B3-Sampled"

parseSpan :: RequestHeaders -> Env -> IO Env
parseSpan headers env =
  let sc = fromMaybe (SpanContext "" HM.empty) $ getAttr spanContextKey env
  in case Prelude.lookup hTraceId headers of
      Just tid -> let sc' = sc { traceId = tid }
                  in return $ env
                      & setAttr spanContextKey      sc'
                      & go (fromMaybe (traceId sc') $ Prelude.lookup hSpanId headers) sc'
      _        -> do
        c <- newContext
        return $ setAttr spanContextKey c env
  where
    go spanId context env' =
      let name = "-"
          startTime  = undefined
          finishTime = Nothing
          tags       = HM.empty
          logs       = HM.empty
          references = []
      in setAttr spanKey Span{..} env'

traceMw :: Env -> (Span -> App ()) -> Middleware
traceMw env' notify app req resH = do
  env <- parseSpan (requestHeaders req) env'
  runApp env $
    runInSpan (decodeUtf8 (requestMethod req) <> " /" <> T.intercalate "/" (pathInfo req)) notify $ \s@Span{..} -> do
      let SpanContext{..} = context
          tid = decodeUtf8 $ traceId <> "," <> spanId
          v   = L.insert extensionLogKey tid (vault req)
          v'  = L.insert spanKey s v
          rh' = resH . mapResponseHeaders (\hs -> (hTraceId, traceId):(hSpanId, spanId):hs)
          c e = do
            runApp env { reqAttributes = Just v} (logError $ showText e)
            rh' $ whenException e
      liftIO (app req {vault = v'} rh' `catch` c)

traceMiddleware :: TraceConfig -> AppMiddleware
traceMiddleware TraceConfig{..}
  = AppMiddleware $ \env f -> if enabled
    then f (env, traceMw env $ notifier method)
    else f (env, id)

