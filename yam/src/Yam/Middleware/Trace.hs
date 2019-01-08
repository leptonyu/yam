module Yam.Middleware.Trace(
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

instance FromJSON TraceNotifyType where
  parseJSON v = go . T.toLower <$> parseJSON v
    where
      go :: Text -> TraceNotifyType
      go _ = NoTracer

instance FromJSON TraceConfig where
  parseJSON = withObject "TraceConfig" $ \v -> TraceConfig
    <$> v .:? "enabled" .!= True
    <*> v .:? "type"    .!= NoTracer

instance Default TraceConfig where
  def = TraceConfig True NoTracer

notifier :: TraceNotifyType -> Span -> App ()
notifier _ _ = return ()

{-# NOINLINE spanContextKey #-}
spanContextKey :: Key SpanContext
spanContextKey = unsafePerformIO newKey

{-# NOINLINE spanKey #-}
spanKey :: Key Span
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

parseSpan :: RequestHeaders -> Env -> Env
parseSpan headers env =
  let sc = fromMaybe (SpanContext "" HM.empty) $ getAttr spanContextKey env
  in case lookup hTraceId headers of
      Just tid -> let sc' = sc { traceId = decodeUtf8 tid }
                  in env & setAttr spanContextKey      sc'
                         & go (maybe (traceId sc') decodeUtf8 $ lookup hSpanId headers) sc'
      _        -> env
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
traceMw env notify app req resH = runApp (parseSpan (requestHeaders req) env) $
  runInSpan ((decodeUtf8 $ requestMethod req) <> " /" <> T.intercalate "/" (pathInfo req)) notify $ \s@Span{..} -> do
    let SpanContext{..} = context
        tid = traceId <> "," <> spanId
        v   = L.insert extensionLogKey tid (vault req)
        v'  = L.insert spanKey s v
    liftIO $ app req {vault = v'}
      $ resH . mapResponseHeaders (\hs -> (hTraceId,encodeUtf8 traceId):(hSpanId, encodeUtf8 spanId):hs)

traceMiddleware :: TraceConfig -> AppMiddleware
traceMiddleware TraceConfig{..}
  = AppMiddleware $ \env f -> if enabled
    then do
      c <- newContext
      let env' = setAttr spanContextKey c env
      f (env', traceMw env' $ notifier method)
    else f (env, id)

