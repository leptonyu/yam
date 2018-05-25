module Yam.Config.Vault where

import           Yam.Config

import           Data.Default
import           Data.Maybe      (fromMaybe)
import           Data.Monoid
import           Data.Text       (Text, justifyRight, pack)
import           Data.Vault.Lazy
import           Data.Word
import           Numeric         (showHex)
import           System.Random

newtype Box a = Box (Maybe (Key a), Vault)

instance Config (Box a) where
  fetch _ a = Right a
  merge = foldl1 $ \(Box (k, v)) (Box (_, v2)) -> Box (k, union v v2)
  fromFile _ _ = error "Unsupported"
  from _ = error "Unsupported"

instance (Monad m) => HasValue m (Box v) v where
  parse (Box (Just k, v)) = return $ case Data.Vault.Lazy.lookup k v of
    Just  a -> Right a
    Nothing -> Left  "Value Not Found"
  parse _ = return $ Left "Key Not Exists"

instance Default (Box a) where
  def = Box (Nothing, empty)

newBox :: Key a -> Vault -> Box a
newBox k vault = Box (Just k, vault)

emptyBox :: IO (Box a)
emptyBox = do
  k <- newKey
  return $ Box (Just k, empty)

toBox :: Box a -> Box b -> Box a
toBox (Box (k, _)) (Box (_, v)) = Box (k,v)

extracBox :: Box a -> Maybe a
extracBox (Box (Just k, v)) = Data.Vault.Lazy.lookup k v
extracBox _                 = Nothing

extracBoxOrDefault :: a -> Box a -> a
extracBoxOrDefault d b = fromMaybe d $ extracBox b

randomString :: IO Text
randomString = do
  c <- randomIO :: IO Word64
  return $ justifyRight 16 '0' $ pack $ showHex c ""

addFirstVault :: (Monoid a, Eq a) => a -> a -> Key a -> Vault -> Vault
addFirstVault prefix sep k v = insert k (go prefix sep (extracBox $ newBox k v)) v
  where
    go p s (Just n) = if n == mempty then p else p <> s <> n
    go p _ _        = p

addLastVault :: (Monoid a, Eq a) => a -> a -> Key a -> Vault -> Vault
addLastVault prefix sep k v = insert k (go prefix sep (extracBox $ newBox k v)) v
  where
    go p s (Just n) = if n == mempty then p else n <> s <> p
    go p _ _        = p
