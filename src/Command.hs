{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Command (startWorkers) where

import AppConfig (Project(..))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan)
import Control.Exception (catch)
import Data.String (fromString)
import Shelly (shelly, liftIO, bash_, chdir)
import System.FilePath (joinPath)
import Text.Regex (splitRegex, mkRegex)
import Logger (LogHandle)

import qualified AppConfig as AC
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Text as T
import qualified Logger
import qualified System.FileLock as FL

startWorkers :: Int -> Chan Project -> LogHandle -> IO ()
startWorkers n chan h =
  sequence_ workers
  where
    consumer i = do
      Logger.logInfo h "Command" $ "Starting worker " <> (T.pack . show $ i)
      consumeFromChan chan h
    workers =
      fmap (forkIO . consumer) (enumFromTo 1 n)

consumeFromChan :: Chan Project -> LogHandle -> IO ()
consumeFromChan chan h =
  do
    command <- Chan.readChan chan
    executeShellCommand command h `catch` Logger.logCaughtError h "Command"
    consumeFromChan chan h -- continue

{-|
  Executes the command for the given 'Project'.

  These steps are involved:

   * acquires a file lock (".webhook" in the project's directory)
   * switches to the project's directory
   * executes the command via "bash"

  If the program terminate in error, then this function will throw
  an exception.
-}
executeShellCommand :: Project -> LogHandle -> IO ()
executeShellCommand project h =
  withLock $
    unsafeExecuteShellCommand project h
  where
    lockFile = joinPath [T.unpack (AC.directory project), ".webhook"]
    withLock f = FL.withFileLock lockFile FL.Exclusive (const f)

{-|
  Internal API: executes the shell command without a file lock.
-}
unsafeExecuteShellCommand :: Project -> LogHandle -> IO ()
unsafeExecuteShellCommand Project{..} h = shelly $ do
  liftIO $ Logger.logInfo h "Command" $ "Executing: chdir " <> directory
  chdir (fromString . T.unpack $ directory) $
    case commandAndArgs of
      x : xs -> do
        liftIO $ Logger.logInfo h "Command" $ "Executing: " <> command
        bash_ (fromString x) (fmap T.pack xs)
      [] ->
        liftIO $ Logger.logInfo h "Command" "No shell command to execute"
    where
      commandAndArgs = splitRegex (mkRegex "[ \t]+") (T.unpack command)
