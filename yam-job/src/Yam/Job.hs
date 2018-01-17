{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Yam.Job(
    YamJob(..)
  , withJobs
  ) where

import           Yam.App

import           System.Cron

data YamJob = YamJob
  { name :: Text
  , cron :: Text
  , func :: AppM IO ()
  }

withJobs :: (MonadIO m, MonadMask m) => [YamJob] -> AppM m a -> AppM m a
withJobs jobs action = do
    context <- ask
    threads <- liftIO $ execSchedule
                      $ mapM_ (go context) jobs
    let tjobs = zipWith (,) threads jobs
    mapM_ (prt True) tjobs
    action `finally` kill tjobs
    where go context job = flip addJob (cron job) $ do
            reqId <- randomHex 8
            let nm = name (job :: YamJob)
            runAppM context $ withLoggerName (reqId <> " job." <> nm)
                            $ do infoLn $ "Start job " <> nm
                                 func job
                                 infoLn ("End job " <> nm)
          kill ts = mapM_ (prt False) ts >> (liftIO $ mapM_ (killThread.fst) ts)
          prt v (_,job) = do
            let nm = name (job :: YamJob)
                r  = if v then "Register" else "Unregister"
            infoLn $ r <> " job " <> nm <> " with cron '" <> cron job <> "'"
