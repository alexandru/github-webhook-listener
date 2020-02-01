{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Server (run) where

import Control.Concurrent (killThread)
import Control.Exception (bracket, uninterruptibleMask_)
import Control.Concurrent.Chan (Chan)
import Controller (ping, processKey)
import Logger (LogHandle)
import Web.Scotty (scotty)

import qualified AppConfig as Cfg
import qualified Control.Concurrent.Chan as Chan
import qualified Command
import qualified Logger
import qualified Data.Text as T


startServer :: Cfg.All -> Chan Cfg.Project -> LogHandle -> IO ()
startServer config chan h =
  scotty port $
    ping path >>
    processKey path (Cfg.projects config) chan h
  where
    port = fromInteger (toInteger (Cfg.port . Cfg.http $ config))
    path = Cfg.path . Cfg.http $ config


run :: Cfg.All -> LogHandle -> IO ()
run appConfig h = do
  Logger.logInfo h "Main" "Starting server ..."
  -- Message queue used for delaying requests
  chan <- Chan.newChan
  -- Ensures that resources terminate safely
  safeBracket
    -- Workers processing requests asynchronously
    (startWorkers chan)
    -- Terminates workers on server shutdown
    (mapM_ killThreadAndLog)
    -- Starts server for receiving HTTP requests
    (const (startServer appConfig chan h))
  where
    startWorkers chan =
      Command.startWorkers (Cfg.workers . Cfg.runtime $ appConfig) chan h
    killThreadAndLog tid = do
      Logger.logInfo h "Command" $ "Killing worker (" <> (T.pack . show $ tid) <> ")"
      killThread tid

-- | Version of bracket that makes the finalizer uninterruptible
safeBracket
  :: IO a        -- ^ acquisition
  -> (a -> IO b) -- ^ release
  -> (a -> IO c) -- ^ use
  -> IO c
safeBracket ini fin =
  bracket (uninterruptibleMask_ ini) (uninterruptibleMask_ . fin)
