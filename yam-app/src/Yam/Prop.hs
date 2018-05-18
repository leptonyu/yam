{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Yam.Prop(
    PropertySource
  , emptyPropertySource
  , MonadProp(..)
  , getProp
  , getPropOrDefault
  , requiredProp
  , tryLoadYaml
  , loadYaml
  , loadEnv
  , loadCommandLineArgs
  , mergePropertySource
  , runProp
  ) where

import           Yam.Import

import           Data.Aeson          (Result (..), fromJSON)
import           Data.Aeson.Types
import           Data.Either         (either)
import qualified Data.HashMap.Strict as M
import           Data.List           (foldl')
import qualified Data.Text           as T
import           Data.Yaml
import           System.Directory    (doesFileExist, getFileSize)
import           System.Environment  (getArgs, getEnvironment)

type PropertySource = (Text, Value)

emptyPropertySource :: PropertySource
emptyPropertySource = ("DEFAULT", Null)

class Monad m => MonadProp m where
  propertySource :: m PropertySource

type ValueProperty = ReaderT PropertySource

instance Monad m => MonadProp (ValueProperty m) where
  propertySource = ask

data PropException = ParseFailed Text Text
                   | KeyNotFound Text
                   | TypeMismatch Text
                   | FileNotFound FilePath
                   | FileLoadFailed FilePath
                   deriving Show
instance Exception PropException

runProp :: (Monad m) => Value -> ValueProperty m a -> m a
runProp v ma = runReaderT ma ("NO_NAME", v)

getPropOrDefault :: (FromJSON a, MonadThrow m, MonadProp m) => a -> Text -> m a
getPropOrDefault d key = fromMaybe d <$> getProp key

requiredProp :: (FromJSON a, MonadThrow m, MonadProp m) => Text -> m a
requiredProp key = getProp key >>= maybe (throwM $ KeyNotFound key) return

getProp :: (FromJSON a, MonadThrow m, MonadProp m) => Text -> m (Maybe a)
getProp key  = propertySource >>= go key (splitKey key)
  where go :: (FromJSON a, Monad m, MonadThrow m) => Text -> [Text] -> PropertySource -> m (Maybe a)
        go k hs (s,v) = to k s $ foldl' fetch (Just v) hs
        fetch :: Maybe Value -> Text -> Maybe Value
        fetch  Nothing      _ = Nothing
        fetch  (Just v)     h = fetch' v h
        fetch' Null         _ = Just Null
        fetch' (Object m) h = case M.lookup h m of
          Just v  -> Just v
          Nothing -> Just Null
        fetch' _          _ = Nothing
        to :: (FromJSON a, Monad m, MonadThrow m) => Text -> Text -> Maybe Value -> m (Maybe a)
        to k _ Nothing     = throwM $ TypeMismatch k
        to _ _ (Just Null) = return Nothing
        to k _ (Just v)    = case fromJSON v of
          Error   e -> throwM $ ParseFailed k $ cs e
          Success a -> return (Just a)
splitKey :: Text -> [Text]
splitKey k | T.null k  = []
           | otherwise = T.split (=='.') k

tryLoadYaml :: (MonadIO m, MonadThrow m) => FilePath -> m (Maybe PropertySource)
tryLoadYaml file = do
  exists <- liftIO $ doesFileExist file
  if exists
    then do
      size <- liftIO $ getFileSize file
      if size > 0 then
        Just . (cs file,) <$> (liftIO (decodeFileEither file) >>= either (\_ -> throwM $ FileLoadFailed file) return)
      else return Nothing
    else return Nothing

loadYaml :: (MonadIO m, MonadThrow m) => FilePath -> m PropertySource
loadYaml file = tryLoadYaml file >>= maybe (throwM $ FileNotFound file) return

loadEnv :: (MonadIO m) => m PropertySource
loadEnv = do
  env <- liftIO getEnvironment
  return ("System.Environment", convertValue env)

loadCommandLineArgs :: (MonadIO m) => m PropertySource
loadCommandLineArgs = do
  args <- liftIO getArgs
  return ("CommandLine", convertValue $ go args)
  where go      = mapMaybe (select . break (=='='))
        select ("", _)     = Nothing
        select (_, "")     = Nothing
        select (k, '=':vs) = Just (k,vs)
        select kvs         = Just kvs

convertValue :: [(String, String)] -> Value
convertValue = toValue . mapMaybe go
  where go :: (String, String) -> Maybe ([Text], Value)
        go (k,v) = (toKey $ cs k, ) <$> Data.Yaml.decode (cs v)
        toKey    = splitKey . T.toLower . T.map (\a -> if a == '_' then '.' else a)
        to :: ([Text], Value) -> Maybe (Text, [([Text], Value)])
        to (h:hs, v) = Just (h, [(hs,v)])
        to ([],   _) = Nothing
        toValue :: [([Text], Value)] -> Value
        toValue [([],value)] = value
        toValue vs           = Object $ M.map toValue $ M.fromListWith (<>) $ mapMaybe to vs

mergePropertySource :: [PropertySource] -> PropertySource
mergePropertySource = foldl' merge emptyPropertySource
  where merge :: (Text, Value) -> (Text, Value) -> (Text, Value)
        merge (n1,v1) (_,v2)        = (n1, merge' v1 v2)
        merge' Null    a             = a
        merge' (Object a) (Object b) = Object (M.unionWith merge' a b)
        merge' a       _             = a
