module Yam.Util where

import           Control.Monad.Logger.CallStack
import           Data.Text                      (Text, justifyRight, pack)
import           Data.Word
import           Numeric
import           System.Random

randomString :: Int -> IO Text
randomString n = do
  c <- randomIO :: IO Word64
  return $ justifyRight n '0' $ pack $ showHex c ""


showText :: Show a => a -> Text
showText = pack . show

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()
