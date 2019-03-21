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
startYam ac@AppConfig{..} sw@SwaggerConfig{..} logConfig enableDefaultMiddleware vs middlewares proxy server =
  withLogger name logConfig $ do
    logInfo $ "Start Service [" <> name <> "] ..."
    logger <- askLoggerIO
    let at = runAM $ foldr1 (<>) ((if enableDefaultMiddleware then defaultMiddleware else []) <> middlewares)
    at (putLogger logger $ Env L.empty Nothing ac) $ \(env, middleware) -> do
      let cxt      = env :. EmptyContext
          pCxt     = Proxy @'[Env]
          portText = showText port
          settings = defaultSettings
                   & setPort port
                   & setOnException (\_ _ -> return ())
                   & setOnExceptionResponse whenException
                   & setSlowlorisSize slowlorisSize
      when enabled $
        logInfo    $ "Swagger enabled: http://localhost:" <> portText <> "/" <> pack urlDir
      logInfo      $ "Servant started on port(s): "       <> portText
      lift
        $ runSettings settings
        $ middleware
        $ serveWithContextAndSwagger sw (baseInfo name vs port) (Proxy @(Vault :> api)) cxt
        $ \v -> hoistServerWithContext proxy pCxt (transApp v env) server

transApp :: Vault -> Env -> App a -> Handler a
transApp v b = liftIO . runApp b . local (\env -> env { reqAttributes = Just v})

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
