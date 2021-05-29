{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Gerrit stream event data type
module Gerrit.Event
  ( User (..),
    Change (..),
    PatchSet (..),
    Ref (..),
    ChangeEventType (..),
    ChangeEvent (..),
    RefEventType (..),
    RefEvent (..),
    CommentEventType (..),
    CommentEvent (..),
    AbandonEventType (..),
    AbandonEvent (..),
    Event (..),
  )
where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Options (fieldLabelModifier), Value (String), defaultOptions, genericParseJSON)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

aesonOption :: Text -> Options
aesonOption prefix = defaultOptions {fieldLabelModifier = recordToJson}
  where
    recordToJson = updateCase . drop (Text.length prefix)
    updateCase [] = []
    updateCase (x : xs) = toLower x : xs

data ChangeEventType
  = PatchsetCreated
  | ChangeMerged
  deriving (Show)

instance FromJSON ChangeEventType where
  parseJSON (String v) = case v of
    "patchset-created" -> pure PatchsetCreated
    "change-merged" -> pure ChangeMerged
    _ -> mzero
  parseJSON _ = mzero

data ProjectEventType
  = ProjectCreated
  deriving (Show)

instance FromJSON ProjectEventType where
  parseJSON (String v) = case v of
    "project-created" -> pure ProjectCreated
    _ -> mzero
  parseJSON _ = mzero

data RefEventType
  = RefUpdated
  deriving (Show)

instance FromJSON RefEventType where
  parseJSON (String v) = case v of
    "ref-updated" -> pure RefUpdated
    _ -> mzero
  parseJSON _ = mzero

data User = User
  { userName :: Maybe Text,
    userEmail :: Maybe Text,
    userUsername :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON $ aesonOption "user"

data PatchSet = PatchSet
  { patchSetNumber :: Int,
    patchSetUploader :: User
  }
  deriving (Show, Generic)

instance FromJSON PatchSet where
  parseJSON = genericParseJSON $ aesonOption "patchSet"

data Change = Change
  { changeProject :: Text,
    changeBranch :: Text,
    changeSubject :: Text,
    changeUrl :: Text,
    changeOwner :: User
  }
  deriving (Show, Generic)

instance FromJSON Change where
  parseJSON = genericParseJSON $ aesonOption "change"

data ChangeEvent = ChangeEvent
  { changeEventProject :: Text,
    changeEventType :: ChangeEventType,
    changeEventUploader :: Maybe User,
    changeEventChange :: Change,
    changeEventPatchSet :: PatchSet
  }
  deriving (Show, Generic)

instance FromJSON ChangeEvent where
  parseJSON = genericParseJSON $ aesonOption "changeEvent"

data CommentEventType
  = CommentAdded
  deriving (Show)

instance FromJSON CommentEventType where
  parseJSON (String v) = case v of
    "comment-added" -> pure CommentAdded
    _ -> mzero
  parseJSON _ = mzero

data CommentEvent = CommentEvent
  { commentEventProject :: Text,
    commentEventType :: CommentEventType,
    commentEventAuthor :: User,
    commentEventChange :: Change,
    commentEventPatchSet :: PatchSet
  }
  deriving (Show, Generic)

instance FromJSON CommentEvent where
  parseJSON = genericParseJSON $ aesonOption "commentEvent"

data AbandonEventType
  = ChangeAbandoned
  deriving (Show)

instance FromJSON AbandonEventType where
  parseJSON (String v) = case v of
    "change-abandoned" -> pure ChangeAbandoned
    _ -> mzero
  parseJSON _ = mzero

data AbandonEvent = AbandonEvent
  { abandonEventProject :: Text,
    abandonEventType :: AbandonEventType,
    abandonEventAbandoner :: User,
    abandonEventChange :: Change,
    abandonEventPatchSet :: PatchSet
  }
  deriving (Show, Generic)

instance FromJSON AbandonEvent where
  parseJSON = genericParseJSON $ aesonOption "abandonEvent"

data ProjectEvent = ProjectEvent
  { projectEventProjectName :: Text,
    projectEventHeadName :: Text,
    projectEventType :: ProjectEventType
  }
  deriving (Show, Generic)

instance FromJSON ProjectEvent where
  parseJSON = genericParseJSON $ aesonOption "projectEvent"

data Ref = Ref
  { refOldRev :: Text,
    refNewRev :: Text,
    refRefName :: Text,
    refProject :: Text
  }
  deriving (Show, Generic)

instance FromJSON Ref where
  parseJSON = genericParseJSON $ aesonOption "ref"

data RefEvent = RefEvent
  { refEventSubmitter :: Maybe User,
    refEventRefUpdate :: Ref,
    refEventType :: RefEventType
  }
  deriving (Show, Generic)

instance FromJSON RefEvent where
  parseJSON = genericParseJSON $ aesonOption "refEvent"

data Event
  = EventChange ChangeEvent
  | EventComment CommentEvent
  | EventProject ProjectEvent
  | EventRef RefEvent
  | EventAbandon AbandonEvent
  deriving (Show)

instance FromJSON Event where
  parseJSON v =
    EventChange <$> parseJSON v
      <|> EventComment <$> parseJSON v
      <|> EventProject <$> parseJSON v
      <|> EventRef <$> parseJSON v
      <|> EventAbandon <$> parseJSON v
