{-# LANGUAGE OverloadedStrings #-}

module Yam.App.Context(
    YamContext(..)
  , HasYamContext(..)
  , requireExtension
  , getExtensionOrDefault
  , getExtension
  , setExtension
  , lockExtenstion
  , emptyContext
  , cleanContext
  ) where

import           Yam.Import
import           Yam.Logger

import qualified Control.Concurrent.Map as M
import           Data.Dynamic

type YamExtension = M.Map Text Dynamic

data YamContext = YamContext
  { defLogger  :: LoggerConfig
  , extensions :: YamExtension
  }

emptyContext :: IO YamContext
emptyContext = YamContext <$> stdoutLogger <*> M.empty

class MonadIO m => HasYamContext m where
  yamContext :: m YamContext

extensionLockKey :: Text
extensionLockKey = "Extension.Lock"

extension :: HasYamContext m => m YamExtension
extension = extensions <$> yamContext

requireExtension :: (HasYamContext m, Typeable a) => Text -> m a
requireExtension key = extension >>= liftIO . M.lookup key >>= get . (fromDynamic =<<)
  where get Nothing  = error $ "Module " <> cs key <> " not loaded"
        get (Just r) = return r

getExtension :: (HasYamContext m, Typeable a) => Text -> m (Maybe a)
getExtension key = (fromDynamic =<<) <$> (extension >>= liftIO . M.lookup key)

getExtensionOrDefault :: (HasYamContext m, Typeable a) => a -> Text -> m a
getExtensionOrDefault a key = (fromMaybe a . (fromDynamic =<<)) <$> (extension >>= liftIO . M.lookup key)

setExtension :: (MonadLogger m, HasYamContext m, Typeable a) => Text -> a -> m ()
setExtension key a = do
  when (extensionLockKey /= key)
    checkLock
  void $ extension >>= liftIO . M.insert key (toDyn a)
  when (extensionLockKey /= key)
    (debugLn $ "Register extension <<" <> key <> ">>")

checkLock :: HasYamContext m => m ()
checkLock = getExtensionOrDefault False extensionLockKey >>= go
  where go True = error "Extension has freezed, cannot modify now"
        go _    = return ()

lockExtenstion :: (MonadLogger m, HasYamContext m)  => m ()
lockExtenstion = setExtension extensionLockKey True

unlockExtenstion :: (MonadLogger m, HasYamContext m)  => m ()
unlockExtenstion = setExtension extensionLockKey False

cleanContext :: (MonadLogger m, HasYamContext m)  => m () -> m ()
cleanContext action = unlockExtenstion >> action
