module Yam.Web.Middleware where

import           Yam.Logger

import           Control.Exception
    ( SomeException
    , catch
    , fromException
    )
import           Data.Default
import           Data.String.Conversions              (cs)
import           Data.Vault.Lazy
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           Servant.Server
import           Servant.Server.Internal.ServantErr   (responseServantErr)

prepareMiddleware :: (Vault -> IO Vault) -> Middleware
prepareMiddleware pre app req resH = do
  vault' <- pre $ vault req
  app req {vault = vault'} resH

errorMiddleware :: (Request -> SomeException -> IO Response) -> Middleware
errorMiddleware f app req resH = app req resH `catch` (\e -> f req e >>= resH)

loggerMiddleware :: LoggerConfig -> IO Middleware
loggerMiddleware lc@LoggerConfig{..} = mkRequestLogger def {destination=Callback $ logger lc {names = "Request":names} INFO}

stdLoggerMiddleware :: IO Middleware
stdLoggerMiddleware = stdoutLoggerConfig >>= loggerMiddleware

servantErrorMiddleware :: LoggerConfig -> Middleware
servantErrorMiddleware lc = errorMiddleware $ \_ e -> do
  errorLn lc (cs $ show e)
  return . responseServantErr $ case fromException e :: Maybe ServantErr of
    Nothing  -> err400 { errBody = cs $ show e }
    Just err -> err
