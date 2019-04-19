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
  ) where

import qualified Control.Category               as C
import           Control.Monad.Logger.CallStack
import           Data.Opentracing
import           Network.Wai
import           Salak
import           Servant
import           Servant.Swagger
import           Yam.App
import           Yam.Config
import           Yam.Logger
import           Yam.Middleware.Error
import           Yam.Middleware.Trace
import           Yam.Prelude
import           Yam.Swagger

-- | Application Middleware.
newtype AppMiddleware a b = AppMiddleware
  { runAM :: Context a -> Middleware -> (Context b -> Middleware -> LoggingT IO ()) -> LoggingT IO () }

instance C.Category AppMiddleware where
  id = AppMiddleware $ \a m f -> f a m
  (AppMiddleware fbc) . (AppMiddleware fab) = AppMiddleware $ \a m f -> fab a m $ \b m1 -> fbc b m1 f

-- | Simple Application Middleware, just provide a config to context.
simpleContext :: a -> AppMiddleware cxt (a ': cxt)
simpleContext a = AppMiddleware $ \c m f -> f (a :. c) m

-- | Simple Application Middleware, just provide a config to context.
simpleConfig' :: (HasSalak cxt, FromProp a) => Text -> (a -> AppT cxt (LoggingT IO) b) -> AppMiddleware cxt (b ': cxt)
simpleConfig' key g = AppMiddleware $ \c m f -> runAppT c (require key) >>= \a -> runAppT c (g a) >>= \b -> f (b :. c) m

-- | Simple Application Middleware, just provide a config to context.
simpleConfig :: (HasSalak cxt, FromProp a) => Text -> AppMiddleware cxt (a ': cxt)
simpleConfig key = simpleConfig' key return

-- | Simple Application Middleware, promote a 'Middleware' to 'AppMiddleware'
simpleMiddleware :: Middleware -> AppMiddleware cxt cxt
simpleMiddleware m = AppMiddleware $ \c m2 f -> f c (m . m2)

-- | Standard Starter of Yam.
start
  :: forall api cxt
  . ( HasServer api cxt
    , HasSwagger api)
  => AppConfig -- ^ Application Config
  -> SwaggerConfig -- ^ SwaggerConfig
  -> Version -- ^ Application Version
  -> IO LogConfig -- ^ Logger Config
  -> (Span -> AppV cxt IO ()) -- ^ Opentracing notifier
  -> AppMiddleware Simple cxt -- ^ Application Middleware
  -> (AppConfig -> Application -> IO ()) -- ^ Run Application
  -> Proxy api -- ^ Application API Proxy
  -> ServerT api (AppV cxt IO) -- ^ Application API Server
  -> IO ()
start ac@AppConfig{..} sw@SwaggerConfig{..} vs logConfig f am runHttp p api =
  withLogger name logConfig $ \logger -> do
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
-- > import           Salak
-- > import           Salak.Yaml
-- > import           Servant
-- > import           Yam
-- > import qualified Control.Category    as C
-- > import           Data.Version
-- >
-- > type API = "hello" :> Get '[PlainText] Text
-- >
-- > service :: ServerT API AppSimple
-- > service = return "world"
-- >
-- > main = runSalakWith "app" YAML $ do
-- >   al <- require  "yam.application"
-- >   sw <- require  "yam.swagger"
-- >   lc <- requireD "yam.logging"
-- >   start al sw (makeVersion []) lc spanNoNotifier emptyAM serveWarp (Proxy @API) service

