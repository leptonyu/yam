module Yam.Web(
    module Yam.Web.Internal
  , module Yam.Web.Swagger
  , module Yam.Web.Middleware
  , runDemo
  ) where

import           Yam.Web.Internal         hiding (API', mkServe')
import           Yam.Web.Middleware
import           Yam.Web.Swagger

import           Data.Proxy
import           Network.Wai.Handler.Warp (run)
import           Servant

runDemo :: IO ()
runDemo = do
  ys <- defaultYamSettings
  run 8080 $ mkServeWithSwagger ys (Proxy :: Proxy EmptyAPI) undefined
