{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Payload (
  Payload(..),
  PAction(..),
  PRef(..),
  ValidateResult(..),
  parsePayload,
  validatePayload,
  actionLog,
  refLog) where

import AppConfig (Project)
import Control.Monad (mfilter)
import Data.Aeson (FromJSON, decode)
import Data.Digest.Pure.SHA (showDigest, hmacSha1)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import GHC.Generics

import qualified AppConfig as C
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as DTLE

{-|
  Request received via GitHub's Webhooks.

  See: <https://developer.github.com/webhooks/>
-}
data Payload = Payload
  {
    action :: Maybe PAction,
    ref :: Maybe PRef
  } deriving (Show, Generic)

instance FromJSON Payload

{-|
  GitHub repository action being performed.

  E.g. "opened". N.B. the "push" event doesn't have an "action"
  signaled in the payload.
-}
newtype PAction = PAction Text
  deriving (Eq, Show, Generic)

instance FromJSON PAction

-- | Unpacks a "Maybe" action for logging purposes.
actionLog :: Maybe PAction -> Text
actionLog Nothing = "<nothing>"
actionLog (Just (PAction a)) = a

{-|
  Git ref, used to identify the branch or tag being acted upon.
  E.g. "refs/heads/gh-pages"
-}
newtype PRef = PRef Text
  deriving (Eq, Show, Generic)

instance FromJSON PRef

-- | Unpacks a "Maybe" ref for logging purposes.
refLog :: Maybe PRef -> Text
refLog Nothing = "<nothing>"
refLog (Just (PRef r)) = r

{-|
  Validation result indicating what to do next.

  Used by the controller.
-}
data ValidateResult =
    Execute              -- ^ Command can be executed
  | SkipExecution        -- ^ Failed conditions (e.g. ref / action)
  | FailedAuthentication -- ^ Failed authentication (e.g. signature) 
  deriving (Eq, Show)

-- | For parsing a text into a 'Payload'
parsePayload :: Text -> Maybe Payload
parsePayload text =
  decode (DTLE.encodeUtf8 (fromStrict text)) 

validatePayload :: Project -> Maybe Payload -> Maybe Text -> Text -> ValidateResult
validatePayload _ Nothing _ _ = SkipExecution
validatePayload project (Just p) sigHead body =
  if not (isAuthenticated (C.secret project) sigHead body) then
     FailedAuthentication
  else if (action p) /= expectedAction then
    SkipExecution
  else if (ref p) /= expectedRef then
    SkipExecution
  else
    Execute
  where
    expectedAction = 
      PAction <$> mfilter (\a -> a /= "push") (C.action project)
    expectedRef =
      PRef <$> (C.ref project)

isAuthenticated :: Maybe Text -> Maybe Text -> Text -> Bool
isAuthenticated key sigHead body =
  case key of
    Nothing -> True
    Just k ->
      case sigHead of
        Just sig ->
          let (header, rest) = T.splitAt 5 sig in
          header == "sha1=" && rest == (T.pack digest)
        _ ->
          False
      where
        digest =
          let digestKey = DTLE.encodeUtf8 . fromStrict $ k in
          let digestBody = DTLE.encodeUtf8 . fromStrict $ body in
          showDigest (hmacSha1 digestKey digestBody)
