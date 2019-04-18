{-# LANGUAGE NoPolyKinds #-}
module Yam(
    start
  , AppConfig(..)
  , AppT
  , AppV
  , AppIO
  , AppSimple
  , getEntry
  , runAppT
  , runVault
  , throwS
  , throw
  -- * Logger
  , LogConfig(..)
  , AppMiddleware(..)
  , simpleAppMiddleware
  , HasContextEntry(..)
  , TryContextEntry(..)
  , HasLogger
  , LogFuncHolder
  , VaultHolder
  -- * Modules
  -- ** Swagger
  , module Yam.Swagger
  -- ** Trace
  , module Yam.Middleware.Trace
  -- * Reexport
  , Span(..)
  , showText
  , randomString
  , randomCode
  , decodeUtf8
  , encodeUtf8
  , pack
  , liftIO
  , fromMaybe
  ) where

import qualified Control.Category               as C
import           Control.Monad.Logger.CallStack
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Swagger
import           Yam.App
import           Yam.Config
import           Yam.Logger
import           Yam.Middleware.Error
import           Yam.Middleware.Trace
import           Yam.Prelude
import           Yam.Swagger

newtype AppMiddleware a b = AppMiddleware
  { runAM :: Context a -> Middleware -> (Context b -> Middleware -> LoggingT IO ()) -> LoggingT IO () }

instance C.Category AppMiddleware where
  id = AppMiddleware $ \a m f -> f a m
  (AppMiddleware fbc) . (AppMiddleware fab) = AppMiddleware $ \a m f -> fab a m $ \b m1 -> fbc b m1 f

simpleAppMiddleware :: a -> AppMiddleware cxt (a ': cxt)
simpleAppMiddleware a = AppMiddleware $ \ c m f -> f (a :. c) m

start
  :: forall api cxt
  . ( HasServer api cxt
    , HasSwagger api)
  => AppConfig
  -> SwaggerConfig
  -> Version
  -> IO LogConfig
  -> (Span -> AppV cxt IO ())
  -> AppMiddleware '[LogFuncHolder] cxt
  -> Proxy api
  -> ServerT api (AppV cxt IO)
  -> IO ()
start AppConfig{..} sw@SwaggerConfig{..} vs logConfig f am p api =
  withLogger name logConfig $ \logger -> runAM am (LF logger :. EmptyContext) id $ \cxt middleware -> do
    logInfo $ "Start Service [" <> name <> "] ..."
    let portText = showText port
        settings = defaultSettings
                 & setPort port
                 & setOnException (\_ _ -> return ())
                 & setOnExceptionResponse whenException
                 & setSlowlorisSize slowlorisSize
    when enabled $
      logInfo    $ "Swagger enabled: http://localhost:" <> portText <> "/" <> pack urlDir
    logInfo      $ "Servant started on port(s): "       <> portText
    liftIO
      $ runSettings settings
      $ traceMiddleware (\v -> runAppT (VH v :. cxt) . f)
      $ middleware
      $ errorMiddleware (LF logger :. EmptyContext)
      $ serveWithContextAndSwagger sw (baseInfo name vs port) (Proxy @(Vault :> api)) cxt
      $ \v -> hoistServerWithContext p (Proxy @cxt) (nt cxt v) api

type AppSimple = AppV '[LogFuncHolder] IO
