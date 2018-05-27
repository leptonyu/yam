module Yam.Web.Middleware where

import           Yam.Config.Vault
import           Yam.Logger

import           Control.Exception
    ( SomeException
    , catch
    , fromException
    )
import           Control.Monad                      ((>=>))
import           Data.Maybe
import           Data.Monoid                        ((<>))
import           Data.String.Conversions            (cs)
import           Data.Text                          (Text)
import           Data.Vault.Lazy
import           Network.Wai
import           Network.Wai.Header
import           Servant.Server
import           Servant.Server.Internal.ServantErr (responseServantErr)

prepareMiddleware :: (Vault -> IO Vault) -> Middleware
prepareMiddleware pre app req resH = do
  vault' <- pre $ vault req
  app req {vault = vault'} resH

errorMiddleware :: (Request -> SomeException -> IO Response) -> Middleware
errorMiddleware f app req resH = app req resH `catch` (f req >=> resH)

apacheMiddleware :: LoggerConfig -> Middleware
apacheMiddleware lc app req sendResponse = app req $ \res -> do
    let msize   = contentLength (responseHeaders res)
        logstr  = "\""
                <> toLogStr (requestMethod req)
                <> " "
                <> toLogStr (rawPathInfo req)
                <> toLogStr (rawQueryString req)
                <> " "
                <> toLogStr (show $ httpVersion req)
                <> "\" "
                <> toLogStr (maybe "-" show msize)
                <> " \""
                <> toLogStr (fromMaybe "" $ requestHeaderReferer req)
                <> "\" \""
                <> toLogStr (fromMaybe "" $ requestHeaderUserAgent req)
                <> "\"\n"
    logger lc {logVault = vault req} INFO logstr
    sendResponse res

stdLoggerMiddleware :: IO Middleware
stdLoggerMiddleware = apacheMiddleware <$> stdoutLoggerConfig

servantErrorMiddleware :: LoggerConfig -> Middleware
servantErrorMiddleware lc = errorMiddleware $ \req e -> do
  logger lc {logVault = vault req} ERROR (toLogStr $ show e)
  return . responseServantErr $ fromMaybe err400 { errBody = cs $ show e } (fromException e :: Maybe ServantErr)

traceMiddleware :: Key Text -> Middleware
traceMiddleware k = prepareMiddleware $ \vault -> do
  traceId <- randomString
  return $ insert k traceId vault
