{-# LANGUAGE OverloadedStrings #-}

module Controller (ping, processKey) where

import Control.Concurrent.Chan (Chan, writeChan)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Logger (LogHandle)
import Network.HTTP.Types (status200, status404, status204, status403)
import Web.Scotty

import qualified AppConfig as Cfg
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Lazy as DTL
import qualified Logger
import qualified Payload as P

ping :: DT.Text -> ScottyM ()
ping path =
  get (fromString $ DT.unpack path) $
    html "Pong!"

processKey :: DT.Text -> Map Text Cfg.Project -> Chan Cfg.Project -> LogHandle -> ScottyM ()
processKey path projects chan h =
  post (fromString route) $ do
    key <- param "key"
    liftIO $ Logger.logInfo h "Controller" $ "POST " <> path <> key
    case Map.lookup key projects of
      Nothing -> send404 key
      Just project -> process key project
  where
    route = DT.unpack (path <> ":key")
    
    process key project = do
      b <- body
      hubSig <- getHeader "X-Hub-Signature"
      let bodyText = DTL.toStrict $ decodeUtf8 b      
      let payload = P.parsePayload bodyText
      let action = payload >>= P.action
      let ref = payload >>= P.ref

      case P.validatePayload project payload hubSig bodyText of
        P.Execute -> do
          liftIO $ Logger.logInfo h "Controller" $ "Executing shell command for key: " <> key          
          liftIO $ writeChan chan project
          status status200
          text "Ok"          
        P.SkipExecution -> do
          liftIO $ Logger.logInfo h "Controller" $
            "Nothing to do for key: " <>  key <> ", action: " 
            <> P.actionLog action <> " and ref: " 
            <> P.refLog ref
          status status204
        P.FailedAuthentication -> do
          liftIO $ Logger.logWarn h "Controller" $ "Validation failed for signature: " <> (DT.pack . show $ hubSig)
          status status403
          text "403 Forbidden"
      
    send404 key = do
      liftIO $ Logger.logWarn h "Controller" $ "404 Not Found: " <> key
      status status404
      text "404 Not Found"

    getHeader name =
      fmap (DTE.decodeUtf8 . lazyToStrict . encodeUtf8) <$>
        header name
      where
        lazyToStrict = BS.concat . BSL.toChunks
