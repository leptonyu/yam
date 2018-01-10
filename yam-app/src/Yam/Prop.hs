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

instance (Monad m) => MonadProp (ValueProperty m) where
  propertySource = ask

runProp :: (Monad m) => Value -> ValueProperty m a -> m a
runProp v ma = runReaderT ma ("NO_NAME", v)

getPropOrDefault :: (FromJSON a, MonadProp m) => a -> Text -> m a
getPropOrDefault def key = fromMaybe def <$> getProp key

requiredProp :: (FromJSON a, MonadProp m) => Text -> m a
requiredProp key = getProp key >>= maybe (error $ "key " <> cs key <> " not found") return

getProp :: (FromJSON a, MonadProp m) => Text -> m (Maybe a)
getProp key  = propertySource >>= go (splitKey key)
  where go :: (FromJSON a, Monad m) => [Text] -> PropertySource -> m (Maybe a)
        go hs (s,v) = to s $ foldl' fetch v hs
        fetch :: Value -> Text -> Value
        fetch (Object map) h = fromMaybe Null $ M.lookup h map
        fetch _            _ = Null
        to :: (FromJSON a, Monad m) => Text -> Value -> m (Maybe a)
        to _ Null = return Nothing
        to s v    = case fromJSON v of
          Error   e -> error e
          Success a -> return (Just a)
splitKey :: Text -> [Text]
splitKey k | T.null k  = []
           | otherwise = T.split (=='.') k

tryLoadYaml :: (MonadIO m) => FilePath -> m (Maybe PropertySource)
tryLoadYaml file = do
  exists <- liftIO $ doesFileExist file
  if exists
    then do
      size <- liftIO $ getFileSize file
      if size > 0 then
        Just . (cs file,) <$> (liftIO (decodeFile file) >>= maybe (error $ file <> " load failed") return)
      else return Nothing
    else return Nothing

loadYaml :: (MonadIO m) => FilePath -> m PropertySource
loadYaml file = tryLoadYaml file >>= maybe (error $ file <> " not found") return

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
        toValue vs           = Object $ M.map toValue $ M.fromListWith (++) $ mapMaybe to vs

mergePropertySource :: [PropertySource] -> PropertySource
mergePropertySource = foldl' merge emptyPropertySource
  where merge :: (Text, Value) -> (Text, Value) -> (Text, Value)
        merge (n1,v1) (n2,v2)        = (n1, merge' v1 v2)
        merge' Null    a             = a
        merge' (Object a) (Object b) = Object (M.unionWith merge' a b)
        merge' a       _             = a
