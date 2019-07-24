{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoPolyKinds           #-}
-- |
-- Module:      Yam
-- Copyright:   (c) 2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- A out-of-the-box wrapper of [servant](https://hackage.haskell.org/package/servant-server),
-- providing configuration loader [salak](https://hackage.haskell.org/package/salak) and flexible extension with 'AppMiddleware'.
--
module Yam(
  -- * How to use this library
  -- $use

  -- * Yam Server
    start
  -- ** Application Configuration
  , AppConfig(..)
  -- ** Application Context
  , AppT
  , runAppT
  , throwS
  -- ** Application Middleware
  , AppMiddleware(..)
  -- * Modules
  -- ** Logger
  , HasBase
  , HasLogger
  , HasSalak
  , HasCxt
  , askCxt
  , LogConfig(..)
  , LogFuncHolder
  , VaultHolder
  -- * Reexport
  , Span(..)
  , SpanContext(..)
  , SpanTag(..)
  , SpanReference(..)
  , showText
  , randomString
  , randomCode
  , decodeUtf8
  , encodeUtf8
  , pack
  , liftIO
  , fromMaybe
  , logInfo
  , logError
  , logWarn
  , logDebug
  ) where

import           Control.Exception              (catch)
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Data.Opentracing
import           Data.Text                      (pack)
import           Network.Wai
import           Salak
import           Servant
import           Servant.Swagger
import           Yam.Internal
import           Yam.Middleware
import           Yam.Middleware.Actuator
import           Yam.Middleware.Swagger
import           Yam.Middleware.Trace

-- | Standard Starter of Yam.
start :: forall m api cxt apicxt app.
  ( m ~ LoggingT (RunSalakT IO)
  , apicxt ~ '[BaseCxt, cxt]
  , app ~ (AppT (Context apicxt) IO)
  , Monad m
  , HasServer  api apicxt
  , HasSwagger api)
  => LoadSalak ()
  -> Version
  -> Middleware
  -> AppMiddleware m apicxt BaseCxt cxt
  -> Proxy api
  -> ServerT api app
  -> IO ()
start load vs md appmd proxy server = loadAndRunSalak load $ do
  app@AppConfig{..} <- require "application"
  c                 <- require "logging"
  withLogger name c $ \logger -> do
    sp <- askSalak
    let baseCxt e = BaseCxt sp (LogFuncHolder logger) (VaultHolder e) app
    logInfo $ "Start Service [" <> name <> "] ..."
    re <- liftIO $ emptyAMTD md
    runAM (defaultMiddleware proxy vs >> appmd) (baseCxt Nothing) re $ \cxt AMTD{..} -> do
      let portText = showText port
          apicxt d = baseCxt d :. cxt :. EmptyContext
      logInfo      $ "Servant started on port(s): "       <> portText
      liftIO
        $ serveWarp app
        $ traceMiddleware (\_ _ -> return ())
        $ errorMiddleware apicxt
        $ middleware
        $ serveYam
          (Proxy @(Vault :> api))
          (apicxt Nothing)
          (\v -> hoistServerWithContext proxy (Proxy @apicxt) (runNT $ apicxt $ Just v) server)

errorMiddleware :: HasLogger cxt => (Maybe Vault -> cxt) -> Middleware
errorMiddleware cxt app req resH = app req resH `catch` (\e -> runAppT (cxt $ Just $ vault req) (logError $ pack $ show e) >> resH (whenException e))

-- | default http server by warp.
serveWarp :: AppConfig -> Application -> IO ()
serveWarp AppConfig{..} = runSettings
  $ defaultSettings
  & setPort port
  & setOnException (\_ _ -> return ())
  & setOnExceptionResponse whenException
  & setSlowlorisSize slowlorisSize

defaultMiddleware v p = actuatorMiddleware >> swaggerMiddleware v p id


-- $use
--
-- > import           Salak.Yaml
-- > import           Servant
-- > import           Yam
-- > import           Data.Version
-- >
-- > type API = "hello" :> Get '[PlainText] Text
-- >
-- > service :: ServerT API AppSimple
-- > service = return "world"
-- >
-- > main = start (loadSalak def) (makeVersion []) id (return ()) (Proxy @API) service

