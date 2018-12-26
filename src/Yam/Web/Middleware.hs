module Yam.Web.Middleware where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Logger.CallStack
import qualified Data.ByteString.Lazy.Char8         as B
import           Data.Maybe
import           Data.Text                          (Text, pack)
import           Data.Vault.Lazy
import           Network.Wai
import           Servant
import           Servant.Server.Internal.ServantErr (responseServantErr)
import           Yam.Util

prepareMiddleware :: IO Vault -> Middleware
prepareMiddleware pre app req resH = do
  v <- pre
  app req { vault = vault req `union` v } resH

errorMiddleware :: (Request -> SomeException -> IO Response) -> Middleware
errorMiddleware f app req resH = app req resH `catch` (f req >=> resH)

traceMiddleware :: Key Text -> Middleware
traceMiddleware k = prepareMiddleware $ do
  traceId <- randomString 16
  return $ insert k traceId empty

servantErrorMiddleware :: (LoggingT IO Response -> IO Response) -> Middleware
servantErrorMiddleware lc = errorMiddleware $ \_ e -> lc $ do
  logError (pack $ show e)
  return . responseServantErr $ fromMaybe
    err400 { errBody = B.pack $ show e }
    (fromException e :: Maybe ServantErr)
