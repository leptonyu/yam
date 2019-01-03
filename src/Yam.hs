{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Yam(
    start
  , App
  , YamConfig(..)
  , DataSourceProvider
  , DataSourceConfig(..)
  , showText
  , throwS
  , runDb
  , selectValue
  , withLogger
  , LogConfig(..)
  ) where
import           Control.Exception              hiding (Handler)
import           Control.Monad.Except
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8     as B
import           Data.Maybe
import           Data.Text                      (Text)
import           Data.Text.Encoding             (encodeUtf8)
import           Network.Wai
import           Network.Wai.Handler.Warp       (run)
import           Yam.Config
import           Yam.DataSource
import           Yam.Logger
import           Yam.Util
import           Yam.Web.Swagger

type YamPayload = (YamConfig, Text, Maybe DataSource)

type AppM m = LoggingT (ReaderT YamPayload m)

type App = AppM IO

throwS :: ServantErr -> Text -> App a
throwS e msg = do
  logError msg
  lift $ throw e

runDb :: DB App a -> App a
runDb a = do
  (_,_, ds) <- ask
  case ds of
    Nothing -> throwS err401 "DataSource not found"
    Just d  -> runDB d a

runApp :: YamPayload -> LogFunc -> App a -> Handler a
runApp (yc,_,ds) lf app = do
  (t, res :: Either SomeException a) <- liftIO $ do
    tid <- randomString 12
    let nlf = addTrace lf tid
    (tid,) <$> try (runReaderT (runLoggingT app nlf) (yc,tid,ds))
  case res of
    Left  e -> throwError $ add t $ fromMaybe err400 { errBody = B.pack $ show e } (fromException e :: Maybe ServantErr)
    Right a -> return a
  where
    add t e = e { errHeaders = ("X-TRACE-ID", encodeUtf8 t): errHeaders e}

start
  :: (HasSwagger api, HasServer api '[YamConfig])
  => YamConfig
  -> Proxy api
  -> ServerT api App
  -> [Middleware]
  -> Maybe DataSourceProvider
  -> App a
  -> LoggingT IO ()
start conf@YamConfig{..} proxy server middleWares newDs appa
  = let cxt      = (conf :. EmptyContext)
  in do
    logInfo "Start Service..."
    runLogger <- askLoggerIO
    tryRunDb newDs datasource runLogger
      $ \ds -> do
        _ <- runHandler $ runApp (conf,"-",ds) runLogger appa
        run port
          $ foldr (.) id middleWares
          $ serveWithContextAndSwagger swagger proxy cxt
          $ hoistServerWithContext proxy (Proxy :: Proxy '[YamConfig]) (runApp (conf,"-",ds) runLogger) server
  where
    tryRunDb (Just d) ds r a = do
      logInfo "Initialize Datasource..."
      liftIO $ runInDB r d ds (a.Just)
    tryRunDb _  _        _ a = liftIO $ a Nothing
