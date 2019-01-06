module Yam.Trace where

import qualified Data.Vault.Lazy  as L
import           Network.Wai
import           System.IO.Unsafe (unsafePerformIO)
import           Yam.Types

type TraceLog = Text

{-# NOINLINE traceKey #-}
traceKey :: Key TraceLog
traceKey = unsafePerformIO newKey

traceMw :: Middleware
traceMw app req resH = do
  traceId <- randomString 12
  let h = ("X-TRACE-ID",encodeUtf8 traceId)
  app req {vault = L.insert traceKey traceId (vault req)} (resH . mapResponseHeaders (h:))

traceMiddleware :: Bool -> AppMiddleware
traceMiddleware enabled = simpleWebMiddleware (enabled, "Trace") traceMw
