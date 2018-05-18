{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Yam.Servant(
    App
  , ServantWrapException(..)
  , API
  , MkApplication
  , ApiToApplication
  , emptyApplication
  , mkServe
  , addApi
  , startSimpleJob
  , startMain
  , start
  , InitializeYamContext
  , DataSourceProviders
  , MigrateSQL
  , LoadYamJobs
  ) where

import           Yam.App
import           Yam.Job
import           Yam.Transaction.Sqlite

import           Control.Exception
    ( SomeException
    , catch
    , fromException
    )
import           Control.Lens                      hiding (Context)
import           Data.Aeson
import           Data.Swagger                      hiding
    ( Header
    , HeaderName
    , port
    )
import qualified Data.Text                         as T
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.AddHeaders (addHeaders)
import           Servant
import           Servant.Server
import           Servant.Server.Internal           (responseServantErr)
import           Servant.Swagger
import           Servant.Swagger.UI

type App = AppM Servant.Handler

data ServantWrapException = forall e. Exception e => Wrap ServantErr e

instance Show ServantWrapException where
  show (Wrap _ e) = show e
instance Exception ServantWrapException

exceptionHandler ::(MonadIO m) => (Text -> m ())
                               -> (m ResponseReceived -> IO ResponseReceived)
                               -> SomeException
                               -> Application
exceptionHandler lg action e _ resH = action $ do
  lg     $ showText e
  liftIO $ resH $ responseServantErr $ go e
  where go :: SomeException -> ServantErr
        go e'  = case fromException e' :: Maybe ServantWrapException of
          Just (Wrap err _) -> err
          _                 -> err400

-- add Correlation-Id and exception convert
middleWare :: YamContext -> Middleware
middleWare context app req resH = do
  reqId   <- randomHex 8
  let go a = addHeaders [("X-Correlation-Id",cs reqId)] a req resH
      run  = runAppM context
  run $ withLoggerName (reqId <> " corn")
      $ liftIO
      $ go app `catch` (go . exceptionHandler errorLn run)

type API api = (Proxy api, YamContext -> Server api)
type MkApplication = YamContext -> Application
type ApiToApplication = forall s. (HasServer s '[YamContext], HasSwagger s) => API s -> MkApplication
type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

emptyApplication :: MkApplication
emptyApplication = mkServe (Proxy :: Proxy EmptyAPI) undefined

mkServe :: (HasServer api '[YamContext], HasSwagger api) => Proxy api -> ServerT api App -> MkApplication
mkServe p api = mkServe' (toAPI p api)
  where
    mkServe' :: (HasServer api '[YamContext], HasSwagger api) => API api -> MkApplication
    mkServe' ps c req resH = do
      enabled   <- evalPropOrDefault True     c "swagger.enable"
      swaggertp <- evalPropOrDefault Jensoleg c "swagger.type"
      if enabled
        then go (swagger swaggertp ps) c req resH
        else go ps                     c req resH
      where go :: (HasServer api '[YamContext]) => API api -> YamContext -> Application
            go (p,s)    c = serveWithContext p (c :. EmptyContext) $ s c

    toAPI' :: (HasServer api '[YamContext]) => YamContext -> Proxy api -> ServerT api App -> Server api
    toAPI' c p = hoistServerWithContext p (Proxy :: Proxy '[YamContext]) (runAppM c :: App a -> Servant.Handler a)

    toAPI :: (HasServer api '[YamContext]) => Proxy api -> ServerT api App -> API api
    toAPI p api = (p, \c -> toAPI' c p api)


addApi :: (HasServer api '[YamContext], HasSwagger api, HasServer new '[YamContext], HasSwagger new)
       => API api -> Bool -> API new -> ApiToApplication -> MkApplication
addApi a ok b f c | ok = f (a `ap` b) c
                  | otherwise = f a c
                  where ap :: API a -> API b -> API (a :<|> b)
                        ap (_,a) (_,b) = (Proxy, \c -> a c:<|>b c)

data SwaggerServiceType = Default | Jensoleg

instance FromJSON SwaggerServiceType where
  parseJSON v = go <$> parseJSON v
    where go :: Text -> SwaggerServiceType
          go "default" = Default
          go _         = Jensoleg

swagger :: (HasServer api '[YamContext], HasSwagger api) => SwaggerServiceType -> API api -> API (SwaggerAPI :<|> api)
swagger tp (proxy, api) = (Proxy, \c -> go tp (swaggerDocument proxy) :<|> api c)
  where go Jensoleg = jensolegSwaggerSchemaUIServer
        go _        = swaggerSchemaUIServer

swaggerDocument :: HasSwagger api => Proxy api -> Swagger
swaggerDocument proxy = toSwagger proxy
                & info.title       .~ "Yam Servant API"
                & info.version     .~ "2018.1"
                & info.contact     ?~ Contact (Just "Daniel YU") Nothing (Just "i@icymint.me")
                & info.description ?~ "This is an API for Yam Project"

data Config = Config
  { port :: Int
  , mode :: RunMode
  } deriving Show

instance FromJSON Config where
  parseJSON (Object v) = do
      scPort   <- v .:? "port" .!= port def
      scMode   <- v .:? "mode" .!= mode def
      return $ Config scPort scMode
  parseJSON v          = typeMismatch "Config" v

instance Default Config where
  def = Config 8888 Development

type InitializeYamContext = YamContext -> IO YamContext
type DataSourceProviders  = [DataSourceProvider (AppM IO) ()]
type MigrateSQL           = AppM IO ()
type LoadYamJobs          = AppM IO [YamJob]

startSimpleJob :: InitializeYamContext -> MigrateSQL -> LoadYamJobs -> IO ()
startSimpleJob i m l = startMain i [sqliteProvider] m l emptyApplication

showConf :: AppM IO ()
showConf = do
  conf :: Config           <- getPropOrDefault def   ""
  rank :: LogRank          <- getPropOrDefault DEBUG "log.level"
  ds   :: DataSource       <- getPropOrDefault def   "datasource"
  ds2  :: Maybe DataSource <- getProp          "datasource.secondary"
  let title = "---------- Run Alert In " <> showText (mode conf) <> " Mode ----------"
      url   = "http://localhost:"        <> showText (port conf)
  infoLn      title
  infoLn    $ "  LogLevel    : "         <> showText rank
  infoLn    $ "  Database    : "         <> showText (dbtype ds)
  infoLn    $ "  ConnStr     : "         <> showText (conn   ds)
  when (conn ds /= ":memory:") $
    infoLn  $ "  Thread      : "         <> showText (thread ds)
  forM_ ds2 $ \d2 -> do
    infoLn    " Secondary DB : "
    infoLn  $ "  Database    : "         <> showText (dbtype d2)
    infoLn  $ "  ConnStr     : "         <> showText (conn   d2)
    when (conn d2 /= ":memory:") $
     infoLn $ "  Thread      : "         <> showText (thread d2)
  infoLn    $ "  URL         : "         <> url
  swagger <- getPropOrDefault True "swagger.enable"
  unless (swagger && mode conf /= Production) $
    infoLn  $ "  SwaggerURL  : "        <> url <> "/swagger-ui"
  infoLn    $ T.replicate (T.length title) "-"

start :: MkApplication -> IO ()
start = startMain return [sqliteProvider] (return ()) (return [])

startMain :: InitializeYamContext
          -> DataSourceProviders
          -> MigrateSQL
          -> LoadYamJobs
          -> MkApplication
          -> IO ()
startMain initialize providers migrateSql jobs application = do
  context <- defaultContext >>= initialize
  runAppM context $ go
  where go :: AppM IO ()
        go = do showConf
                mds  <- getPropOrDefault def "datasource"
                ds2nd<- getProp              "datasource.secondary"
                conf <- getPropOrDefault def ""
                initDataSource providers mds ds2nd $ do
                  when (migrate mds && mode conf /= Production) migrateSql
                  lockExtension
                  jbs <- jobs
                  withJobs jbs $ do
                    context <- ask
                    logger  <- toWaiLogger
                    let pt       = port (conf :: Config)
                        settings = setPort pt
                                 $ setLogger logger defaultSettings
                    liftIO $ runSettings settings
                           $ middleWare  context
                           $ application context

