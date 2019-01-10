module Yam.Middleware.Default where

import           Network.Wai.Middleware.Gzip
import           Yam.Middleware
import           Yam.Middleware.Trace

defaultMiddleware :: AppMiddleware
defaultMiddleware = foldr1 (<>)
  [ simpleWebMiddleware (True, "Gzip") $ gzip def
  , traceMiddleware def
  ]
