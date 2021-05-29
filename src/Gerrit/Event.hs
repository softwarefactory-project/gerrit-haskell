{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Gerrit stream event data type
module Gerrit.Event where

import Control.Monad (mzero)
import Data.Aeson
  ( FromJSON (..),
    Options (fieldLabelModifier),
    Value (Object, String),
    defaultOptions,
    genericParseJSON,
    (.:),
  )
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

data PatchsetCreated = PatchsetCreated
  { patchsetCreatedProject :: Text,
    patchsetCreatedUploader :: User,
    patchsetCreatedChange :: Change,
    patchsetCreatedPatchSet :: PatchSet
  }
  deriving (Show, Generic)

instance FromJSON PatchsetCreated where
  parseJSON = genericParseJSON $ aesonOption "patchsetCreated"

data ChangeMerged = ChangeMerged
  { changeMergedProject :: Text,
    changeMergedSubmitter :: User,
    changeMergedChange :: Change,
    changeMergedPatchSet :: PatchSet
  }
  deriving (Show, Generic)

instance FromJSON ChangeMerged where
  parseJSON = genericParseJSON $ aesonOption "changeMerged"

data CommentAdded = CommentAdded
  { commentAddedProject :: Text,
    commentAddedAuthor :: User,
    commentAddedChange :: Change,
    commentAddedPatchSet :: PatchSet
  }
  deriving (Show, Generic)

instance FromJSON CommentAdded where
  parseJSON = genericParseJSON $ aesonOption "commentAdded"

data ChangeAbandoned = ChangeAbandoned
  { changeAbandonedProject :: Text,
    changeAbandonedAbandoner :: User,
    changeAbandonedChange :: Change,
    changeAbandonedPatchSet :: PatchSet
  }
  deriving (Show, Generic)

instance FromJSON ChangeAbandoned where
  parseJSON = genericParseJSON $ aesonOption "changeAbandoned"

data ProjectCreated = ProjectCreated
  { projectCreatedProjectName :: Text,
    projectCreatedHeadName :: Text
  }
  deriving (Show, Generic)

instance FromJSON ProjectCreated where
  parseJSON = genericParseJSON $ aesonOption "projectCreated"

data Ref = Ref
  { refOldRev :: Text,
    refNewRev :: Text,
    refRefName :: Text,
    refProject :: Text
  }
  deriving (Show, Generic)

instance FromJSON Ref where
  parseJSON = genericParseJSON $ aesonOption "ref"

data RefUpdated = RefUpdated
  { refUpdatedSubmitter :: Maybe User,
    refUpdatedRefUpdate :: Ref
  }
  deriving (Show, Generic)

instance FromJSON RefUpdated where
  parseJSON = genericParseJSON $ aesonOption "refUpdated"

data EventType
  = PatchsetCreatedEvent
  | ChangeMergedEvent
  | ChangeAbandonedEvent
  | CommentAddedEvent
  | ProjectCreatedEvent
  | RefUpdatedEvent
  deriving (Show, Eq)

instance FromJSON EventType where
  parseJSON (String v) = case v of
    "patchset-created" -> pure PatchsetCreatedEvent
    "change-merged" -> pure ChangeMergedEvent
    "change-abandoned" -> pure ChangeAbandonedEvent
    "comment-added" -> pure CommentAddedEvent
    "project-created" -> pure ProjectCreatedEvent
    "ref-updated" -> pure RefUpdatedEvent
    _ -> mzero
  parseJSON _ = mzero

data Event
  = EventPatchsetCreated PatchsetCreated
  | EventChangeMerged ChangeMerged
  | EventChangeAbandoned ChangeAbandoned
  | EventCommentAdded CommentAdded
  | EventProjectCreated ProjectCreated
  | EventRefUpdated RefUpdated
  deriving (Show)

instance FromJSON Event where
  parseJSON o@(Object v) = do
    eType <- v .: "type"
    case eType of
      PatchsetCreatedEvent -> EventPatchsetCreated <$> parseJSON o
      ChangeMergedEvent -> EventChangeMerged <$> parseJSON o
      ChangeAbandonedEvent -> EventChangeAbandoned <$> parseJSON o
      CommentAddedEvent -> EventCommentAdded <$> parseJSON o
      ProjectCreatedEvent -> EventProjectCreated <$> parseJSON o
      RefUpdatedEvent -> EventRefUpdated <$> parseJSON o
  parseJSON _ = mzero

getChange :: Event -> Maybe Change
getChange event = case event of
  EventPatchsetCreated PatchsetCreated {..} -> Just patchsetCreatedChange
  EventChangeMerged ChangeMerged {..} -> Just changeMergedChange
  EventChangeAbandoned ChangeAbandoned {..} -> Just changeAbandonedChange
  EventCommentAdded _ -> Nothing
  EventProjectCreated _ -> Nothing
  EventRefUpdated _ -> Nothing

getUser :: Event -> Maybe User
getUser event = case event of
  EventPatchsetCreated PatchsetCreated {..} -> Just patchsetCreatedUploader
  EventChangeMerged ChangeMerged {..} -> Just changeMergedSubmitter
  EventChangeAbandoned ChangeAbandoned {..} -> Just changeAbandonedAbandoner
  EventCommentAdded _ -> Nothing
  EventProjectCreated _ -> Nothing
  EventRefUpdated _ -> Nothing

-- | Get the 'EventType' back from an 'Event'
getEventType :: Event -> EventType
getEventType event = case event of
  EventPatchsetCreated _ -> PatchsetCreatedEvent
  EventChangeMerged _ -> ChangeMergedEvent
  EventChangeAbandoned _ -> ChangeAbandonedEvent
  EventCommentAdded _ -> CommentAddedEvent
  EventProjectCreated _ -> ProjectCreatedEvent
  EventRefUpdated _ -> RefUpdatedEvent
