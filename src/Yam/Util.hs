module Yam.Util where

import           Data.Text     (Text, justifyRight, pack)
import           Data.Word
import           Numeric
import           System.Random

randomString :: Int -> IO Text
randomString n = do
  c <- randomIO :: IO Word64
  return $ justifyRight n '0' $ pack $ showHex c ""

showText :: Show a => a -> Text
showText = pack . show
