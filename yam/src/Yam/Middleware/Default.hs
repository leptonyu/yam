module Yam.Middleware.Default where

import           Yam.Middleware
import           Yam.Middleware.Client
import           Yam.Middleware.Trace
import           Yam.Types.Prelude

defaultMiddleware :: [AppMiddleware]
defaultMiddleware =
  [ traceMiddleware  def
  , clientMiddleware def
  ]
