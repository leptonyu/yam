{-# LANGUAGE OverloadedStrings #-}

module Yam.Logger.WaiLogger where

import           Yam.Import
import           Yam.Logger

import           Network.Wai.Logger

toWaiLogger :: (MonadLogger m) => m ApacheLogger
toWaiLogger = do mkLogger <- flip runReaderT <$> loggerConfig
                 liftIO   $  apacheLogger
                         <$> initLogger FromFallback (LogCallback (mkLogger . logL INFO) $ return ()) (return "")
