{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Maybe
import qualified Data.Salak             as S
import           Data.Text              (Text)
import           Test.Hspec
import           Test.QuickCheck
import           Yam.Config
import           Yam.DataSource

main = hspec spec

spec :: Spec
spec = do
  describe "Yam.Config" specConfig

specConfig = do
  context "configuration" $ do
    it "load" $ do
      p <- S.defaultPropertiesWithFile "yam_test.yml"
      let get :: S.FromProperties a => Text -> Maybe a
          get = flip S.lookup p
      let dsf = get "yam.datasource" :: Maybe DataSourceConfig
      shouldSatisfy dsf isJust
      let conf = get "yam" :: Maybe YamConfig
      shouldSatisfy conf isJust
      datasource <$> conf `shouldBe` dsf
