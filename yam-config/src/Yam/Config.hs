module Yam.Config(
    Config(..)
  , HasValue(..)
  , defaultConfig
  ) where

import           Data.Aeson
import           Data.Foldable           (foldl')
import qualified Data.HashMap.Strict     as H
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import           Data.Text               (Text, splitOn)
import qualified Data.Text               as T
import           Data.Yaml
import           System.Environment

class Config c where
  fetch   :: Text -> c -> Either String c
  merge   :: [c] -> c
  from    ::  (String, String)  -> c
  fromEnv :: [(String, String)] -> c
  fromEnv = merge . fmap from
  fromCommandLine :: [String]   -> c
  fromCommandLine = merge . fmap (from . select . break (=='='))
    where
      select (k, '=':vs) = (k,vs)
      select v           = v
  {-# MINIMAL fetch,merge,from #-}

keys :: Text -> Text -> [Text]
keys sep = dropWhile T.null . splitOn sep

defaultConfig :: Config c => IO c
defaultConfig = do
  args <- getArgs
  envs <- getEnvironment
  return $ merge $ [fromCommandLine args, fromEnv envs]

instance Config Value where
  fetch key = go key (keys "." key)
    where
      go _  []             c  = Right c
      go k' (k:ks) (Object c) = case H.lookup k c of
        Nothing -> Left $ "Key " <> T.unpack k' <> " Not Found"
        Just v  -> go k' ks v
      go k' _              _  = Left $ "Key " <> T.unpack k' <> " Not Match"
  merge = foldl' merge' Null
    where
      merge' Null    a             = a
      merge' (Object a) (Object b) = Object (H.unionWith merge' a b)
      merge' a       _             = a
  from (k,v) = case Data.Yaml.decode $ cs v of
    Nothing -> Null
    Just a  -> go' (keys "_" $ T.toLower $ T.pack k) a
    where
      go' []      v' = v'
      go' (k':ks) v' = Object $ H.insert k' (go' ks v') H.empty

class (Monad m, Config c) => HasValue m c v where
  parse :: c -> m (Either String v)
  getValue :: Text -> c -> m (Either String v)
  getValue k c = case fetch k c of
    Left  e -> return $ Left e
    Right v -> parse v
  getValueOrDef :: v -> Text -> c -> m v
  getValueOrDef v k c = do
    v' <- getValue k c
    case v' of
      Left  _ -> return v
      Right w -> return w
  requireValue :: Text -> c -> m v
  requireValue k c = do
    v' <- getValue k c
    case v' of
      Left  e -> fail e
      Right v -> return v
  {-# MINIMAL parse #-}

instance (Monad m, FromJSON v) => HasValue m Value v where
  parse c = return $ case fromJSON c of
    Success a -> Right a
    Error   e -> Left  e
