{-# LANGUAGE DeriveGeneric #-}

module AppConfig (
  All (..),
  Http (..),
  Project(..),
  Runtime(..),
  readAppConfig) where

import Data.Map (Map)
import Data.String (fromString)
import Data.Text
import Data.Yaml (FromJSON, decodeFileThrow)
import GHC.Generics
import System.Directory (makeAbsolute)

-- | The complete format of the configuration file
data All = All
  {
    http :: Http,
    runtime :: Runtime,
    projects :: Map Text Project
  } deriving (Generic, Show, Eq)

instance FromJSON All

{-|
  Configuration for where the web server will listen for requests
  (e.g. HTTP path and port)
-}
data Http = Http
  {
    path :: Text    -- ^ HTTP path prefix, to attach to all routes (e.g. "/" or "/api/")
  , port :: Integer -- ^ HTTP port to listen on (e.g. 8080)
  } deriving (Generic, Show, Eq)

instance FromJSON Http

-- | Project configuration.
data Project = Project
  {
    action :: Maybe Text -- ^ Identifies the GitHub event, none for "push"
  , ref :: Maybe Text    -- ^ Git ref that we are expecting
  , directory :: Text    -- ^ working local directory (e.g. `/var/www/website.com`)
  , command :: Text      -- ^ shell command to execute
  , secret :: Maybe Text -- ^ key used to sign the request
  } deriving (Generic, Show, Eq)

instance FromJSON Project

{-|
  Configures misc runtime properties
-}
data Runtime = Runtime
  {
    workers :: Int        -- ^ the number of workers to process requests in parallel
  , output :: FilePath    -- ^ where to send the logs to; accepts: stdout, stderr
  } deriving (Generic, Show, Eq)

instance FromJSON Runtime

{-|
  Given a file path, parses its contents into an 'All'
  configuration object.

  Can throw if the file isn't valid.
-}
readAppConfig :: FilePath -> IO All
readAppConfig filePath = do
  absolute <- makeAbsolute filePath
  decodeFileThrow (fromString absolute)
