module Yam.Server.Refresh where

import           Data.Text   (Text, pack)
import           Salak
import           Servant
import           Yam.App
import           Yam.Logger
import           Yam.Prelude

type RefreshEndpoint = "refresh" :> Post '[PlainText] Text

refreshEndpoint :: (HasSalaks cxt, HasLogger cxt, MonadIO m) => Bool -> AppT cxt m Text
refreshEndpoint True = do
  io <- askReload
  ReloadResult{..} <- liftIO io
  if hasError
    then throwS err400 $ showMsg msgs
    else return $ showMsg msgs
refreshEndpoint _     = throwS err401 "Refresh not allowed"

showMsg :: [String] -> Text
showMsg = pack . unlines
