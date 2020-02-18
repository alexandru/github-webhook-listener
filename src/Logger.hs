{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Logger (
    LogHandle
  , Severity(..)
  , logToHandle
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logCaughtError) where

import Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Text.Printf (printf)
import System.IO (Handle, hPutStrLn, stdout, hFlush)

-- | Just an alias for documentation purposes
type LogHandle = Handle

data Severity =
  DEBUG | INFO | WARN | ERROR
  deriving Show

now :: IO Text
now = do
  ts <- getCurrentTime
  let str = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%z" ts
  return (pack str)

logToHandle :: LogHandle -> Severity -> Text -> Text -> IO ()
logToHandle handle severity context message = do
  nowStr <- now
  hPutStrLn handle (printf "[%s] [%s] [%s] %s" nowStr (show severity) context message)
  hFlush handle

logDebug :: Text -> Text -> IO ()
logDebug = logToHandle stdout DEBUG

logInfo :: LogHandle -> Text -> Text -> IO ()
logInfo handle = logToHandle handle INFO

logWarn :: LogHandle -> Text -> Text -> IO ()
logWarn handle = logToHandle handle WARN

logError :: LogHandle -> Text -> Text -> IO ()
logError handle = logToHandle handle ERROR

logCaughtError :: LogHandle -> Text -> IOError -> IO ()
logCaughtError handle ctx err =
  logToHandle handle ERROR ctx (T.pack (show err))
