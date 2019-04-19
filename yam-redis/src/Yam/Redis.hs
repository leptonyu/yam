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
    RedisConfig(..)
  , HasRedis
  , redisMiddleware
  , ttlOpts
  , runR
  , multiE
  , REDIS
  ) where

import           Control.Exception              (bracket)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import qualified Data.ByteString.Char8          as BC
import           Data.Default
import           Data.Menshen
import           Data.Word
import           Database.Redis
import           Salak
import           Servant
import           Yam

data RedisConfig = RedisConfig
  { url            :: String
  , maxConnections :: Word16
  } deriving (Eq, Show)

instance Default RedisConfig where
  def = RedisConfig "redis://localhost/0" 50

instance FromProp RedisConfig where
  fromProp = RedisConfig
    <$> "url"       .?: url ? pattern "^redis://"
    <*> "max-conns" .?: maxConnections

-- | Middleware context type.
newtype REDIS = REDIS Connection
-- | Middleware context.
type HasRedis cxt = (HasLogger cxt, HasContextEntry cxt REDIS)

instance (HasRedis cxt, MonadIO m) => MonadRedis (AppT cxt m) where
  liftRedis a = do
    REDIS conn <- getEntry
    liftIO $ runRedis conn a

instance HasRedis cxt => RedisCtx (AppT cxt Redis) (Either Reply) where
  returnDecode = lift . returnDecode

instance HasRedis cxt => RedisCtx (AppT cxt RedisTx) Queued where
  returnDecode = lift . returnDecode

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
    go (TxAborted  ) = Left $ Error "RedisTx aborted"
    go (TxError   e) = Left $ Error $ BC.pack e

redisMiddleware :: RedisConfig -> AppMiddleware a (REDIS : a)
redisMiddleware RedisConfig{..} = AppMiddleware $ \cxt m f -> do
  logInfo "Redis loaded"
  lf <- askLoggerIO
  case parseConnectInfo url of
    Left er -> error er
    Right c -> liftIO
      $ bracket (connect c { connectMaxConnections = fromIntegral maxConnections }) disconnect
      $ \conn -> runLoggingT (f (REDIS conn :. cxt) m) lf

ttlOpts :: Integer -> SetOpts
ttlOpts seconds = SetOpts (Just seconds) Nothing Nothing
