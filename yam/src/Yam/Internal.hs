{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoPolyKinds         #-}
module Yam.Internal(
  -- * Application Functions
    startYam
  , start
  ) where

import           Data.Salak
import qualified Data.Vault.Lazy          as L
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Swagger
import           Yam.Logger
import           Yam.Middleware
import           Yam.Middleware.Default
import           Yam.Swagger
import           Yam.Types

startYam
  :: forall api. (HasSwagger api, HasServer api '[Env])
  => AppConfig
  -> SwaggerConfig
  -> IO LogConfig
  -> Bool
  -> Version
  -> [AppMiddleware]
  -> Proxy api
  -> ServerT api App
  -> IO ()
startYam ac@AppConfig{..} sw@SwaggerConfig{..} logConfig enableDefaultMiddleware vs middlewares proxy server
  = withLogger name logConfig $ do
      logInfo $ "Start Service [" <> name <> "] ..."
      logger <- askLoggerIO
      let act = runAM $ foldr1 (<>) ((if enableDefaultMiddleware then defaultMiddleware else []) <> middlewares)
      act (putLogger logger $ Env L.empty Nothing ac) $ \(env, middleware) -> do
        let cxt                  = env :. EmptyContext
            pCxt                 = Proxy :: Proxy '[Env]
            portText             = showText port
            proxy'               = Proxy :: Proxy (Vault :> api)
            server'              = runRequest proxy pCxt server
            settings             = defaultSettings
                                 & setPort port
                                 & setOnException (\_ _ -> return ())
                                 & setOnExceptionResponse whenException
                                 & setSlowlorisSize slowlorisSize
        when enabled $
          logInfo $ "Swagger enabled: http://localhost:" <> portText <> "/" <> pack urlDir
        logInfo $ "Servant started on port(s): " <> portText
        lift $ runSettings settings
          $ middleware
          $ serveWithContextAndSwagger sw ac vs proxy' cxt
          $ hoistServerWithContext proxy' pCxt (transApp env) server'

runRequest :: (HasServer api context) => Proxy api -> Proxy context -> ServerT api App -> Vault -> ServerT api App
runRequest p pc a v = hoistServerWithContext p pc go a
  where
    {-# INLINE go #-}
    go :: App a -> App a
    go = local (\env -> env { reqAttributes = Just v})

transApp :: Env -> App a -> Handler a
transApp b c = liftIO $ runApp b c

start
  :: forall api. (HasSwagger api, HasServer api '[Env])
  => Properties
  -> Version
  -> [AppMiddleware]
  -> Proxy api
  -> ServerT api App
  -> IO ()
start p a b c d = do
  (lc,_) <- runLoader p $ (,) <$> load "yam.logging" <*> askSetProperties
  startYam
    (p .>> "yam.application")
    (p .>> "yam.swagger"    )
    lc
    (p .?> "yam.middleware.default.enabled" .|= True)
    a b c d
