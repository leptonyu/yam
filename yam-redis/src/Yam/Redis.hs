{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module:      Yam.Redis
-- Copyright:   (c) 2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Redis supports for [yam](https://hackage.haskell.org/package/yam).
--
module Yam.Redis(
    HasRedis
  , REDIS
  , redisMiddleware
  , ttlOpts
  , runR
  , multiE
  ) where

import           Control.Exception              (bracket)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import qualified Data.ByteString.Char8          as BC
import           Data.Default
import           Data.Word
import           Database.Redis
import           Salak
import           Servant
import           Yam

instance Default ConnectInfo where
  def = defaultConnectInfo

instance FromProp ConnectInfo where
  fromProp = ConnInfo
    <$> "host"      .?: connectHost
    <*> "port"      .?: connectPort
    <*> "password"  .?: connectAuth
    <*> "database"  .?: connectDatabase
    <*> "max-conns" .?: connectMaxConnections
    <*> "max-idle"  .?: connectMaxIdleTime
    <*> return Nothing
    <*> return Nothing

instance FromProp PortID where
  fromProp = PortNumber . fromIntegral <$> (fromProp :: Prop Word16)

-- | Middleware context type.
newtype REDIS = REDIS Connection
-- | Middleware context.
type HasRedis cxt = (HasLogger cxt, HasContextEntry cxt REDIS)

instance (HasRedis cxt, MonadIO m) => MonadRedis (AppT cxt m) where
  liftRedis a = do
    REDIS conn <- getEntry
    liftIO $ runRedis conn a

runR :: (MonadIO m, HasRedis cxt) => Redis (Either Reply a) -> AppT cxt m a
runR a = do
  v   <- liftRedis a
  case v of
    Left  e -> throwS err400 $ showText e
    Right e -> return e

multiE :: RedisTx (Queued a) -> Redis (Either Reply a)
multiE a = go <$> multiExec a
  where
    go (TxSuccess o) = Right o
    go  TxAborted    = Left $ Error "RedisTx aborted"
    go (TxError   e) = Left $ Error $ BC.pack e

redisMiddleware :: RunSalak (AppMiddleware a (REDIS : a))
redisMiddleware = do
  ci <- require "redis"
  return $ AppMiddleware $ \cxt m h f -> do
    logInfo "Redis loaded"
    lf <- askLoggerIO
    liftIO
      $ bracket (connect ci) disconnect
      $ \conn -> runLoggingT (f (REDIS conn :. cxt) m (mergeHealth (go conn) "redis" h)) lf
  where
    go c = runRedis c ping >> return UP

ttlOpts :: Integer -> SetOpts
ttlOpts seconds = SetOpts (Just seconds) Nothing Nothing
