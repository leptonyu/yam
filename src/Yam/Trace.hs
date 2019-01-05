module Yam.Trace where

import qualified Data.Vault.Lazy                   as L
import           Network.Wai.Middleware.AddHeaders
import           System.IO.Unsafe                  (unsafePerformIO)
import           Yam.Types

type TraceLog = Text

{-# NOINLINE traceKey #-}
traceKey :: Key TraceLog
traceKey = unsafePerformIO newKey

traceMw :: Middleware
traceMw app req resH = do
  traceId <- randomString 12
  app req {vault = L.insert traceKey traceId (vault req)} resH

-- addHeaders [("X-TRACE-ID",encodeUtf8 traceId)] $

traceMiddleware :: AppMiddleware
traceMiddleware = AppMiddleware $ \env f -> f (env, traceMw)
