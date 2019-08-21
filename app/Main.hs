{-# LANGUAGE OverloadedStrings #-}

module Main where

import CmdLine
import Server (run)
import System.IO (stdout, stderr, withFile, IOMode(AppendMode))
import qualified AppConfig as Cfg

main :: IO ()
main = do
  args <- getCmdLineArgs
  appConfig <- Cfg.readAppConfig (configPath args)
  let output = Cfg.output . Cfg.runtime $ appConfig
  case output of
    "stdout" ->
      run appConfig stdout
    "stderr" ->
      run appConfig stderr
    path ->
      withFile path AppendMode (run appConfig)
