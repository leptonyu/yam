{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE NoPolyKinds         #-}
module Yam.Internal(
  -- * Application Functions
    startYam
  , start
  , App
  , module Yam.Logger
  , module Yam.Types
  , SwaggerConfig(..)
  -- * Utilities
  , throwS
  , readConf
  ) where

import           Control.Exception          hiding (Handler)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Salak                 as S
import qualified Data.Vault.Lazy            as L
import           GHC.Stack
import           Network.Wai.Handler.Warp   (run)
import           Servant.Swagger
import           Yam.Logger
import           Yam.Swagger
import           Yam.Trace
import           Yam.Types

type App = AppM IO

runApp :: LogFunc -> Env -> App a -> Handler a
runApp a b c = do
  res :: Either SomeException a <- liftIO $ try (runAppM a b c)
  case res of
    Left  e -> throwError $ fromMaybe err400 { errBody = B.pack $ show e } (fromException e :: Maybe ServantErr)
    Right r -> return r

throwS :: (HasCallStack, MonadIO m) => ServantErr -> Text -> AppM m a
throwS e msg = do
  logErrorCS ?callStack msg
  lift $ throw e

startYam
  :: forall api. (HasSwagger api, HasServer api '[Env])
  => AppConfig
  -> SwaggerConfig
  -> LogConfig
  -> Bool
  -> [AppMiddleware]
  -> Proxy api
  -> ServerT api App
  -> IO ()
startYam ac@AppConfig{..} sw@SwaggerConfig{..} logConfig enableTrace middlewares proxy server
  = withLogger name logConfig $ do
      logInfo $ "Start Service [" <> name <> "] ..."
      logger <- askLoggerIO
      (runAM $ foldr1 (<>) (traceMiddleware enableTrace : middlewares)) (Env L.empty Nothing ac) $ \(env, middleware) -> do
        let cxt                  = env :. EmptyContext
            pCxt                 = Proxy :: Proxy '[Env]
            portText             = showText port
            proxy'               = Proxy :: Proxy (Vault :> api)
            server'              = runRequest proxy pCxt server
        when enabled $
          logInfo $ "Swagger enabled: http://localhost:" <> portText <> "/" <> pack urlDir
        logInfo $ "Servant started on port(s): " <> portText
        lift $ run port
          $ middleware
          $ serveWithContextAndSwagger sw proxy' cxt
          $ hoistServerWithContext proxy' pCxt (runApp logger env) server'

runRequest :: (HasServer api context) => Proxy api -> Proxy context -> ServerT api App -> Vault -> ServerT api App
runRequest p pc a v = hoistServerWithContext p pc go a
  where
    {-# INLINE go #-}
    go :: App a -> App a
    go b = do
      let trace :: Maybe TraceLog = L.lookup traceKey v
      withAppM (\(env,lf) -> (env { reqAttributes = Just v}, nlf lf trace)) b
    {-# INLINE nlf #-}
    nlf x (Just t) = addTrace x t
    nlf x _        = x

readConf :: (Default a, S.FromProperties a) => Text -> S.Properties -> a
readConf k p = fromMaybe def $ S.lookup k p

start
  :: forall api. (HasSwagger api, HasServer api '[Env])
  => S.Properties
  -> [AppMiddleware]
  -> Proxy api
  -> ServerT api App
  -> IO ()
start p middlewares proxy service = startYam
  (readConf "yam.application" p)
  (readConf "yam.swagger"     p)
  (readConf "yam.logging"     p)
  (fromMaybe True $ S.lookup "yam.trace.enabled" p)
  middlewares proxy service
