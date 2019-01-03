{-# LANGUAGE OverloadedStrings #-}
module Yam.Config where

import           Data.Aeson
import           Data.Default
import           Data.Text
import           Yam.DataSource
import           Yam.Web.Swagger

data YamConfig = YamConfig
  { datasource :: DataSourceConfig
  , swagger    :: SwaggerConfig
  , appName    :: Text
  , port       :: Int
  } deriving (Eq, Show)

instance FromJSON YamConfig where
  parseJSON = withObject "YamConfig" $ \v -> YamConfig
    <$> v .:  "datasource"
    <*> v .:? "swagger" .!= def
    <*> v .:  "application"
    <*> v .:? "port"    .!= 8888

