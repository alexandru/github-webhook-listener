{-# LANGUAGE OverloadedStrings #-}

module CmdLine (
  AppArgs (..),
  getCmdLineArgs) where

import Control.Exception (Exception)
import Control.Exception.Base (throwIO)
import Options.Applicative
import Data.Semigroup ((<>))
import System.Directory (doesFileExist)

-- |Models command line arguments
data AppArgs = AppArgs
  { 
    configPath :: FilePath
  } deriving (Show)

-- |Thrown in case the given `configFile` does not exist
data ConfigFileDoesNotExistsException =
  ConfigFileDoesNotExistsException String
  deriving Show

instance Exception ConfigFileDoesNotExistsException

appArgsParser :: Parser AppArgs
appArgsParser = AppArgs
  <$> strOption
     ( long "config-path"
       <> short 'c'
       <> metavar "CONFIG_PATH"
       <> help "Path to the configuration file"
     )

opts :: ParserInfo AppArgs
opts = info (appArgsParser <**> helper)
  ( fullDesc
    <> progDesc "Starts the web server as configured via the indicated config file"
    <> header "github-webhook-listener - a web server that responds to GitHub's Webhooks" )

{-|
  Parse command line arguments.

  Throws `ConfigFileDoesNotExistsException` in case the given
  `configFile` does not exist.
-}
getCmdLineArgs :: IO AppArgs
getCmdLineArgs = do
  config <- execParser opts
  hasFile <- doesFileExist (configPath config)
  if hasFile then
    return config
  else
    throwIO (ConfigFileDoesNotExistsException (configPath config))
