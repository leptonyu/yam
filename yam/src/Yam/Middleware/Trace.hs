{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yam.Middleware.Trace(
  -- * Trace Middleware
    TraceConfig(..)
  , Span(..)
  , hTraceId
  , hParentTraceId
  , hSpanId
  , hSampled
  , traceMiddleware
  ) where

import           Control.Monad.State
import           Data.Default
import qualified Data.HashMap.Lazy   as HM
import           Data.Opentracing
import qualified Data.Text           as T
import qualified Data.Vault.Lazy     as L
import           Network.HTTP.Types
import           Network.Wai
import           Salak
import           Servant
import           System.IO.Unsafe    (unsafePerformIO)
import           Yam.Logger
import           Yam.Prelude

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
    <$> "enabled" .?: enabled
    <*> "type"    .?: method

instance Default TraceConfig where
  def = TraceConfig True NoTracer

{-# NOINLINE spanContextKey #-}
spanContextKey :: L.Key SpanContext
spanContextKey = unsafePerformIO L.newKey

{-# NOINLINE spanKey #-}
spanKey :: L.Key Span
spanKey = unsafePerformIO L.newKey

hTraceId :: HeaderName
hTraceId = "X-B3-TraceId"

hParentTraceId :: HeaderName
hParentTraceId = "X-B3-ParentSpanId"

hSpanId :: HeaderName
hSpanId = "X-B3-SpanId"

hSampled :: HeaderName
hSampled = "X-B3-Sampled"

parseSpan :: RequestHeaders -> Vault -> IO Vault
parseSpan headers vault =
  let sc = fromMaybe (SpanContext "" HM.empty) $ L.lookup spanContextKey vault
  in case Prelude.lookup hTraceId headers of
      Just tid -> let sc' = sc { traceId = tid }
                  in return $ vault
                      & L.insert spanContextKey sc'
                      & go (fromMaybe tid $ Prelude.lookup hSpanId headers) sc'
      _        -> do
        c <- newContext
        return $ L.insert spanContextKey c vault
  where
    go spanId context vault' =
      let name = "-"
          startTime  = undefined
          finishTime = Nothing
          tags       = HM.empty
          logs       = HM.empty
          references = []
      in L.insert spanKey Span{..} vault'

instance MonadIO m => MonadTracer (StateT Request m) where
  askSpanContext = do
    req <- get
    v   <- liftIO $ parseSpan (requestHeaders req) (vault req)
    put req { vault = v}
    return $ fromJust $ L.lookup spanContextKey v

instance MonadIO m => MonadTracing (StateT Request m) where
  runInSpan name nt a = do
    req <- get
    n   <- case L.lookup spanKey $ vault req of
        Just sp -> newChildSpan name sp
        _       -> newSpan name
    nt n
    a' <- a n
    finishSpan n >>= nt
    return a'

traceMiddleware :: (Span -> IO ()) -> Middleware
traceMiddleware notify app req resH = (`evalStateT` req)
  $ runInSpan (decodeUtf8 (requestMethod req) <> " /" <> T.intercalate "/" (pathInfo req)) (liftIO . notify)
  $ \s@Span{..} -> do
    let SpanContext{..} = context
        tid = decodeUtf8 $ traceId <> "," <> spanId
        v   = L.insert extensionLogKey tid (vault req)
        v'  = L.insert spanKey s v
        rh' = resH . mapResponseHeaders (\hs -> (hTraceId, traceId):(hSpanId, spanId):hs)
    liftIO (app req {vault = v'} rh')

