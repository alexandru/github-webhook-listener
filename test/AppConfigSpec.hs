{-# LANGUAGE OverloadedStrings #-}

module AppConfigSpec (
  appConfigSpec) where

import Test.Hspec
import Paths_github_webhook_listener
import Data.String (fromString)

import AppConfig
import qualified Data.Map as Map

appConfigSpec :: SpecWith ()
appConfigSpec =
  describe "AppConfig" $
    it "can read and parse config" $ do
      file <- getDataFileName "resources/config.yaml"
      cfg <- readAppConfig file
      cfg `shouldBe` allConfig
  where
    allConfig = All {
      http = Http {
        path = fromString "/",
        port = 8080
      },
      runtime = Runtime {
        workers = 2,
        output = "stdout"
      },
      projects = Map.fromList [
        (fromString "myproject", Project {
          action = Nothing,
          ref = Just "refs/heads/gh-pages",
          directory = "/var/www/domain.com",
          command = "git pull",
          secret = Just "xxxxxxxxxxxxxxxxxxxx"
        })
      ]
    }
