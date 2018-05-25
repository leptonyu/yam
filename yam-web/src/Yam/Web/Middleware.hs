module Yam.Web.Middleware where

import           Yam.Config.Vault
import           Yam.Logger

import           Control.Exception
    ( SomeException
    , catch
    , fromException
    )
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
errorMiddleware f app req resH = app req resH `catch` (\e -> f req e >>= resH)

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
    logger lc (vault req) INFO logstr
    sendResponse res

stdLoggerMiddleware :: IO Middleware
stdLoggerMiddleware = apacheMiddleware <$> stdoutLoggerConfig

servantErrorMiddleware :: LoggerConfig -> Middleware
servantErrorMiddleware lc = errorMiddleware $ \req e -> do
  errorLn (addVaultToLoggerConfig (vault req) lc) (cs $ show e)
  return . responseServantErr $ case fromException e :: Maybe ServantErr of
    Nothing  -> err400 { errBody = cs $ show e }
    Just err -> err

traceMiddleware :: Key Text -> Middleware
traceMiddleware k = prepareMiddleware $ \vault -> do
  traceId <- randomString
  return $ insert k traceId vault
