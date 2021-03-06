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
  , start'
  , serveWarp
  -- ** Application Configuration
  , AppConfig(..)
  -- ** Application Context
  , AppT
  , AppV
  , AppIO
  , AppSimple
  , Simple
  , runAppT
  , runVault
  , throwS
  -- ** Application Middleware
  , AppMiddleware(..)
  , emptyAM
  , simpleContext
  , simpleConfig
  , simpleConfig'
  , simpleMiddleware
  -- *** Health
  , HealthStatus(..)
  , HealthResult(..)
  , mergeHealth
  -- * Modules
  -- ** Logger
  , LogConfig(..)
  , HasLogger
  , LogFuncHolder
  , VaultHolder
  -- ** Context
  , Context(..)
  , HasContextEntry(..)
  , TryContextEntry(..)
  , getEntry
  , tryEntry
  -- ** Configuration
  , HasSalaks
  -- ** Swagger
  , SwaggerConfig(..)
  , serveWithContextAndSwagger
  , baseInfo
  , SwaggerTag
  -- * Reexport
  , spanNoNotifier
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
  , throw
  , logInfo
  , logError
  , logWarn
  , logDebug
  ) where

import qualified Control.Category               as C
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Data.Opentracing
import           Data.Text                      (pack)
import           Network.Wai
import           Salak
import           Servant
import           Servant.Swagger
import           Yam.App
import           Yam.Config
import           Yam.Logger
import           Yam.Middleware
import           Yam.Middleware.Error
import           Yam.Middleware.Trace
import           Yam.Prelude
import           Yam.Server
import           Yam.Server.Health
import           Yam.Swagger

-- | Standard Starter of Yam.
start
  :: forall file api cxt
  . ( HasLoad file
    , HasLogger cxt
    , HasSalaks cxt
    , HasServer api  cxt
    , HasSwagger api)
  => String -- ^ File config name
  -> file -- ^ Config file format
  -> Version -- ^ Version
  -> AppMiddleware Simple cxt -- ^ Application Middleware
  -> Proxy api -- ^ Application API Proxy
  -> ServerT api (AppV cxt IO) -- ^ Application API Server
  -> IO ()
start cfg file = start' (loadSalakWith file cfg)

-- | Standard Starter of Yam.
start'
  :: forall api cxt
  .(HasLogger cxt
  , HasSalaks cxt
  , HasServer api  cxt
  , HasSwagger api)
  => LoadSalak ()
  -> Version
  -> AppMiddleware Simple cxt
  -> Proxy api
  -> ServerT api (AppV cxt IO)
  -> IO ()
start' load ver amd pSer ser = loadAndRunSalak load $ do
  app@AppConfig{..}    <- require "application"
  c                    <- require "logging"
  withLogger name c $ \logger -> do
    sw <- require "swagger"
    ac <- require "actuator"
    sp <- askSalak
    let portText = showText port
        baseCxt  = sp :. LF logger :. EmptyContext
    logInfo $ "Start Service [" <> name <> "] ..."
    liftX $ runAM amd baseCxt id emptyHealth $ \cxt middleware hr -> do
      when (enabled (sw :: SwaggerConfig)) $
        logInfo    $ "Swagger enabled: http://localhost:" <> portText <> "/" <> pack (urlDir sw)
      logInfo      $ "Servant started on port(s): "       <> portText
      let go :: forall a. (HasSwagger a, HasServer a cxt) => Proxy a -> ServerT a (AppV cxt IO) -> LoggingT IO ()
          go x y = liftIO
            $ serveWarp app
            $ traceMiddleware (\v -> runAppT (VH v :. cxt) . spanNoNotifier)
            $ middleware
            $ errorMiddleware baseCxt
            $ serveWithContextAndSwagger sw (baseInfo hostname name ver port) (Proxy @(Vault :> a)) cxt
            $ \v -> hoistServerWithContext x (Proxy @cxt) (nt cxt v) y
      if enabled (ac :: ActuatorConfig)
        then go (Proxy @(api :<|> ActuatorEndpoint)) (ser :<|> actuatorEndpoint hr ac)
        else go pSer ser

-- | default http server by warp.
serveWarp :: AppConfig -> Application -> IO ()
serveWarp AppConfig{..} = runSettings
  $ defaultSettings
  & setPort port
  & setOnException (\_ _ -> return ())
  & setOnExceptionResponse whenException
  & setSlowlorisSize slowlorisSize

-- | Empty span notifier.
spanNoNotifier :: Span -> AppV cxt IO ()
spanNoNotifier _ = return ()

-- | Empty Application Middleware.
emptyAM :: AppMiddleware cxt cxt
emptyAM = C.id

-- | Simple Application context
type Simple = '[SourcePack, LogFuncHolder]

-- | Simple Application with logger context.
type AppSimple = AppV Simple IO


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
-- > main = start "app" YAML (makeVersion []) (return emptyAM) (Proxy @API) (return service)

