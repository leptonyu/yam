{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Yam.Job(
    YamJob(..)
  , MonadJob(..)
  , startJob
  ) where

import           Yam.App

import           System.Cron

data YamJob = YamJob
  { name :: Text
  , cron :: Text
  , func :: AppM IO ()
  }

class MonadIO m => MonadJob m where
  yamJobs :: m [(ThreadId, YamJob)]
  registerJob :: YamJob -> m ()
  killJobs :: m ()

keyJob :: Text
keyJob = "Extension.Job"

instance MonadIO m => MonadJob (AppM m) where
  yamJobs = getExtensionOrDefault [] keyJob
  killJobs = yamJobs >>= mapM_ go >> setExtension keyJob ([] :: [(ThreadId, YamJob)])
    where go (tid,job) = do infoLn $ "Stop job " <> name (job :: YamJob) <> "..."
                            liftIO $ killThread tid
  registerJob job = do
    let nm = name (job :: YamJob)
    infoLn $ "Register job " <> nm <> " with cron '" <> cron job <> "'"
    context  <- ask
    [thread] <- liftIO $ execSchedule
                      $ flip addJob (cron job)
                      $ do reqId <- randomHex 8
                           runAppM context $ withLoggerName (reqId <> " job." <> nm)
                                           $ do infoLn $ "Start job " <> nm
                                                func job
                                                infoLn $ "End job " <> nm
    jobs <- yamJobs
    setExtension keyJob ((thread,job):jobs)

startJob :: [YamJob] -> (YamContext -> IO YamContext) -> IO ()
startJob jobs initialize = do
  context <- defaultContext >>= initialize
  runAppM context $ mapM_ registerJob jobs
