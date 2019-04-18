{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yam.Redis(
    RedisConfig(..)
  , HasRedis
  , redisMiddleware
  , ttlOpts
  ) where

import           Control.Exception              (bracket)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Logger.CallStack
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

type HasRedis cxt = HasContextEntry cxt Connection

instance (HasRedis cxt, MonadIO m) => MonadRedis (AppT cxt m) where
  liftRedis a = do
    conn <- getEntry
    liftIO $ runRedis conn a

redisMiddleware :: RedisConfig -> AppMiddleware a (Connection : a)
redisMiddleware RedisConfig{..} = AppMiddleware $ \cxt m f -> do
  logInfo "Redis loaded"
  lf <- askLoggerIO
  case parseConnectInfo url of
    Left er -> error er
    Right c -> liftIO
      $ bracket (connect c { connectMaxConnections = fromIntegral maxConnections }) disconnect
      $ \conn -> runLoggingT (f (conn :. cxt) m) lf

ttlOpts :: Integer -> SetOpts
ttlOpts seconds = SetOpts (Just seconds) Nothing Nothing
