{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoPolyKinds         #-}
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
  -- ** Swagger
  , SwaggerConfig(..)
  , serveWithContextAndSwagger
  , baseInfo
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
import           Yam.Swagger

-- | Standard Starter of Yam.
start'
  :: forall api cxt
  . ( HasServer api cxt
    , HasSwagger api)
  => AppConfig -- ^ Application Config
  -> SwaggerConfig -- ^ SwaggerConfig
  -> Version -- ^ Application Version
  -> LogFunc -- ^ Logger
  -> (Span -> AppV cxt IO ()) -- ^ Opentracing notifier
  -> AppMiddleware Simple cxt -- ^ Application Middleware
  -> (AppConfig -> Application -> IO ()) -- ^ Run Application
  -> Proxy api -- ^ Application API Proxy
  -> ServerT api (AppV cxt IO) -- ^ Application API Server
  -> IO ()
start' ac@AppConfig{..} sw@SwaggerConfig{..} vs logger f am runHttp p api = (`runLoggingT` logger) $ do
  logInfo $ "Start Service [" <> name <> "] ..."
  let portText = showText port
      baseCxt  = LF logger :. EmptyContext
  runAM am baseCxt id $ \cxt middleware -> do
    when enabled $
      logInfo    $ "Swagger enabled: http://localhost:" <> portText <> "/" <> pack urlDir
    logInfo      $ "Servant started on port(s): "       <> portText
    liftIO
      $ runHttp ac
      $ traceMiddleware (\v -> runAppT (VH v :. cxt) . f)
      $ middleware
      $ errorMiddleware baseCxt
      $ serveWithContextAndSwagger sw (baseInfo hostname name vs port) (Proxy @(Vault :> api)) cxt
      $ \v -> hoistServerWithContext p (Proxy @cxt) (nt cxt v) api

-- | Standard Starter of Yam.
start
  :: forall file api cxt
  . ( HasLoad file
    , HasLogger cxt
    , HasServer api  cxt
    , HasSwagger api)
  => String -- ^ File config name
  -> file -- ^ Config file format
  -> Version -- ^ Version
  -> RunSalak (AppMiddleware Simple cxt) -- ^ Application Middleware
  -> Proxy api -- ^ Application API Proxy
  -> RunSalak (ServerT api (AppV cxt IO)) -- ^ Application API Server
  -> IO ()
start cfg file v mkAppMD p mkServer = runSalakWith cfg file $ do
  c   <- requireD "logging"
  app <- require  "application"
  let an = if Yam.Config.name app == "application" then pack cfg <> "-server" else Yam.Config.name app
  withLogger an c $ \logger -> unSalak $ do
    b   <- require  "swagger"
    md' <- mkAppMD
    s'  <- mkServer
    (en, aep) <- actuatorEndpoint
    readLogs >>= mapM_ (logInfo . ("Parsing " <>))
    let go :: forall a. (HasSwagger a, HasServer a cxt) => Proxy a -> ServerT a (AppV cxt IO) -> IO ()
        go = start' app {name = an } b v logger spanNoNotifier md' serveWarp
    liftIO $ if en
      then go (Proxy @(api :<|> ActuatorEndpoint)) (s' :<|> aep)
      else go p s'

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
type Simple = '[LogFuncHolder]

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

