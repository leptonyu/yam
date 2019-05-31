module Yam.Server.Refresh where

import           Data.Text   (Text, pack)
import           Salak
import           Servant
import           Yam.App
import           Yam.Logger
import           Yam.Prelude

type RefreshEndpoint = "refresh" :> Post '[PlainText] Text

refreshEndpoint :: (HasLogger cxt, MonadIO m) => IO ReloadResult -> Bool -> AppT cxt m Text
refreshEndpoint io True = do
  ReloadResult{..} <- liftIO io
  if isError
    then throwS err400 $ showMsg msg
    else return $ showMsg msg
refreshEndpoint _ _     = throwS err401 "Refresh not allowed"

showMsg :: [String] -> Text
showMsg = pack . unlines
