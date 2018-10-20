{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import CmdLine
import Controller
import System.IO (stdout, stderr, withFile, IOMode(AppendMode))
import Logger (LogHandle)

import qualified AppConfig as Cfg
import qualified Command as Command
import qualified Control.Concurrent.Chan as Chan
import qualified Logger as Logger

main :: IO ()
main = do
  args <- getCmdLineArgs
  appConfig <- Cfg.readAppConfig (configPath args)
  let output = (Cfg.output . Cfg.runtime $ appConfig)
  case output of
    "stdout" ->
      run appConfig stdout
    "stderr" ->
      run appConfig stderr
    path ->
      withFile path AppendMode (run appConfig)

run :: Cfg.All -> LogHandle -> IO ()
run appConfig h = do
  Logger.logInfo h "Main" "Starting server ..."
  -- Message queue used for delaying requests
  chan <- Chan.newChan
  -- Workers processing requests asynchronously
  Command.startWorkers (Cfg.workers . Cfg.runtime $ appConfig) chan h
  -- Starts server for receiving HTTP requests
  startServer appConfig chan h
