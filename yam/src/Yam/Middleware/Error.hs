{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ImplicitParams #-}
module Yam.Middleware.Error where

import           Control.Monad.Logger.CallStack
import           Network.Wai
import           Servant
import           Yam.App
import           Yam.Logger
import           Yam.Prelude

errorMiddleware :: HasLogger cxt => Context cxt -> Middleware
errorMiddleware cxt app req resH = app req resH `catch` (\e -> go e >> resH (whenException e))
  where
    go = runVault cxt (vault req) . logErrorCS ?callStack . showText
