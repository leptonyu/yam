{-# LANGUAGE OverloadedStrings #-}

module Yam.Event(
    MonadEvent(..)
  , Event(..)
  , thenNotify
  ) where
import           Yam.Import

import           Yam.Logger

import           Control.Exception (SomeException)
import           Data.Typeable

class (Monad m) => MonadEvent m where
  eventHandler :: Event e => Proxy e -> m [e -> IO ()]

class (ToJSON e, Typeable e) => Event e where
  eventKey :: Proxy e -> String
  eventKey = show . typeRep

thenNotify :: (Event e, MonadYamLogger m, MonadMask m, MonadEvent m) => m a -> e -> m a
thenNotify ma e = do
  a <- ma
  let printStack :: (Event e, MonadYamLogger m) => e -> SomeException -> m ()
      printStack e x = do
          errorLn $ "Event "      <> encodeToText e <> " Failed!"
          errorLn $ "Exception: " <> showText x
  withLoggerName "Event" $ do
    infoLn  $ "Event " <> encodeToText e <> " Received!"
    ec  <- eventHandler (Proxy :: Proxy e)
    mapM_ (\h -> liftIO (h e) `catchAll` printStack e) ec
  return a
