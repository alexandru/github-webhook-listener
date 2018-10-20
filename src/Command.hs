{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Command (startWorkers) where

import AppConfig (Project(..))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan)
import Control.Exception (bracket_, catch, throwIO)
import Data.String (fromString)
import Shelly (shelly, liftIO, bash_, chdir)
import System.Directory (removeFile)
import System.FilePath (joinPath)
import System.IO.Error (isDoesNotExistError)
import Text.Regex (splitRegex, mkRegex)
import Logger (LogHandle)

import qualified AppConfig as AC
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Text as T
import qualified Logger as Logger
import qualified System.FileLock as FL

startWorkers :: Int -> Chan Project -> LogHandle -> IO ()
startWorkers n chan h =
  () <$ sequence workers
  where
    consumer i = do
      Logger.logInfo h "Command" $ "Starting worker " <> (T.pack . show $ i)
      consumeFromChan chan h
    workers =
      fmap (\i -> forkIO $ consumer i) (enumFromTo 1 n)

consumeFromChan :: Chan Project -> LogHandle -> IO ()
consumeFromChan chan h =
  do
    command <- Chan.readChan chan
    executeShellCommand command h `catch` (Logger.logCaughtError h "Command")
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
    lockFile = joinPath [(T.unpack (AC.directory project)), ".webhook"]
    withLock f =
      bracket_
      (return ())
      (removeIfExists lockFile)
      (FL.withFileLock lockFile FL.Exclusive (\_ -> f))
      
{-|
  Internal API: executes the shell command without a file lock.
-}
unsafeExecuteShellCommand :: Project -> LogHandle -> IO ()
unsafeExecuteShellCommand Project{..} h = shelly $ do
  liftIO $ Logger.logInfo h "Command" $ "Executing: chdir " <> directory
  chdir (fromString . T.unpack $ directory) $ do
    case commandAndArgs of
      x : xs -> do
        liftIO $ Logger.logInfo h "Command" $ "Executing: " <> command
        bash_ (fromString x) (fmap T.pack xs)
      [] -> do
        liftIO $ Logger.logInfo h "Command" "No shell command to execute"
    where
      commandAndArgs = splitRegex (mkRegex "[ \t]+") (T.unpack command)

{-|
  Internal API - deletes a file on disk only if it exists
  (without throwing an exception if the path does not exist)
-}
removeIfExists :: FilePath -> IO ()
removeIfExists fileName =
  removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
          