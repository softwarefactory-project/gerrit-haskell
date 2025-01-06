{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Gerrit stream event data type
module Gerrit.Data.Event (
    -- * Main event data types
    EventType (..),
    Event (..),

    -- * Common data types
    User (..),
    PatchSet (..),
    Change (..),
    Ref (..),

    -- * Individual event data types
    AssigneeChanged (..),
    ChangeAbandoned (..),
    ChangeDeleted (..),
    ChangeMerged (..),
    ChangeRestored (..),
    CommentAdded (..),
    HashtagsChanged (..),
    ProjectCreated (..),
    PatchsetCreated (..),
    RefUpdated (..),
    ReviewerAdded (..),
    ReviewerDeleted (..),
    TopicChanged (..),
    WorkInProgressStateChanged (..),
    PrivateStateChanged (..),
    VoteDeleted (..),

    -- * Convenient functions
    getChange,
    getPatchSet,
    getUser,
    getCreatedOn,
    getEventType,
    eventName,
)
where

import Control.Monad (mzero)
import Data.Aeson (
    FromJSON (..),
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
import GHC.Int (Int64)

aesonOption :: Text -> Options
aesonOption prefix = defaultOptions{fieldLabelModifier = recordToJson}
  where
    recordToJson = updateCase . drop (Text.length prefix)
    updateCase [] = []
    updateCase (x : xs) = toLower x : xs

-------------------------------------------------------------------------------
-- Shared types
-------------------------------------------------------------------------------
data User = User
    { userName :: Maybe Text
    , userEmail :: Maybe Text
    , userUsername :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON User where
    parseJSON = genericParseJSON $ aesonOption "user"

data PatchSet = PatchSet
    { patchSetNumber :: Int
    , patchSetRef :: Text
    , patchSetUploader :: User
    , patchsetAuthor :: User
    }
    deriving (Show, Eq, Generic)

instance FromJSON PatchSet where
    parseJSON = genericParseJSON $ aesonOption "patchSet"

data Change = Change
    { changeProject :: Text
    , changeBranch :: Text
    , changeSubject :: Text
    , changeNumber :: Int
    , changeUrl :: Text
    , changeOwner :: User
    , changeWip :: Maybe Bool
    , changeTopic :: Maybe Text
    , changeCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

instance FromJSON Change where
    parseJSON = genericParseJSON $ aesonOption "change"

data Ref = Ref
    { refOldRev :: Text
    , refNewRev :: Text
    , refRefName :: Text
    , refProject :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON Ref where
    parseJSON = genericParseJSON $ aesonOption "ref"

-------------------------------------------------------------------------------
-- Individual Event data types
-------------------------------------------------------------------------------
data AssigneeChanged = AssigneeChanged
    { assigneeChangedChange :: Change
    , assigneeChangedChanger :: User
    , assigneeChangedEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data ChangeAbandoned = ChangeAbandoned
    { changeAbandonedProject :: Text
    , changeAbandonedAbandoner :: User
    , changeAbandonedChange :: Change
    , changeAbandonedPatchSet :: PatchSet
    , changeAbandonedReason :: Maybe Text
    , changeAbandonedEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data ChangeDeleted = ChangeDeleted
    { changeDeletedChange :: Change
    , changeDeletedDeleter :: User
    }
    deriving (Show, Eq, Generic)

data ChangeMerged = ChangeMerged
    { changeMergedProject :: Text
    , changeMergedSubmitter :: User
    , changeMergedChange :: Change
    , changeMergedPatchSet :: PatchSet
    , changeMergedEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data ChangeRestored = ChangeRestored
    { changeRestoredChange :: Change
    , changeRestoredPatchSet :: PatchSet
    , changeRestoredRestorer :: User
    , changeRestoredReason :: Maybe Text
    , changeRestoredEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data CommentAdded = CommentAdded
    { commentAddedChange :: Change
    , commentAddedPatchSet :: PatchSet
    , commentAddedAuthor :: User
    , commentAddedEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data HashtagsChanged = HashtagsChanged
    { hashtagsChangedChange :: Change
    , hashtagsChangedEditor :: User
    , hashtagsChangedEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data ProjectCreated = ProjectCreated
    { projectCreatedProjectName :: Text
    , projectCreatedHeadName :: Text
    , projectCreatedEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data PatchsetCreated = PatchsetCreated
    { patchsetCreatedUploader :: User
    , patchsetCreatedChange :: Change
    , patchsetCreatedPatchSet :: PatchSet
    , patchsetCreatedEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data RefUpdated = RefUpdated
    { refUpdatedSubmitter :: User
    , refUpdatedRefUpdate :: Ref
    , refUpdatedEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data ReviewerAdded = ReviewerAdded
    { reviewerAddedChange :: Change
    , reviewerAddedPatchSet :: PatchSet
    , reviewerAddedAdder :: User
    , reviewerAddedEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data ReviewerDeleted = ReviewerDeleted
    { reviewerDeletedChange :: Change
    , reviewerDeletedPatchSet :: PatchSet
    , reviewerDeletedRemover :: User
    , reviewerDeletedEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data TopicChanged = TopicChanged
    { topicChangedChange :: Change
    , topicChangedChanger :: User
    , topicChangedOldTopic :: Text
    , topicChangedEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data WorkInProgressStateChanged = WorkInProgressStateChanged
    { workInProgressStateChangedChange :: Change
    , workInProgressStateChangedPatchSet :: PatchSet
    , workInProgressStateChangedChanger :: User
    , workInProgressStateChangedEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data PrivateStateChanged = PrivateStateChanged
    { privateStateChangedChange :: Change
    , privateStateChangedPatchSet :: PatchSet
    , privateStateChangedChanger :: User
    , privateStateChangedChangedEventCreatedOn :: Int64
    }
    deriving (Show, Eq, Generic)

data VoteDeleted = VoteDeleted
    { voteDeletedChange :: Change
    , voteDeletedPatchSet :: PatchSet
    , voteDeletedRemover :: User
    }
    deriving (Show, Eq, Generic)

-------------------------------------------------------------------------------
-- Events FromJSON instances
-------------------------------------------------------------------------------
instance FromJSON AssigneeChanged where
    parseJSON = genericParseJSON $ aesonOption "assigneeChanged"

instance FromJSON ChangeAbandoned where
    parseJSON = genericParseJSON $ aesonOption "changeAbandoned"

instance FromJSON ChangeDeleted where
    parseJSON = genericParseJSON $ aesonOption "changeDeleted"

instance FromJSON ChangeMerged where
    parseJSON = genericParseJSON $ aesonOption "changeMerged"

instance FromJSON ChangeRestored where
    parseJSON = genericParseJSON $ aesonOption "changeRestored"

instance FromJSON CommentAdded where
    parseJSON = genericParseJSON $ aesonOption "commentAdded"

instance FromJSON HashtagsChanged where
    parseJSON = genericParseJSON $ aesonOption "hashtagsChanged"

instance FromJSON ProjectCreated where
    parseJSON = genericParseJSON $ aesonOption "projectCreated"

instance FromJSON PatchsetCreated where
    parseJSON = genericParseJSON $ aesonOption "patchsetCreated"

instance FromJSON RefUpdated where
    parseJSON = genericParseJSON $ aesonOption "refUpdated"

instance FromJSON ReviewerAdded where
    parseJSON = genericParseJSON $ aesonOption "reviewerAdded"

instance FromJSON ReviewerDeleted where
    parseJSON = genericParseJSON $ aesonOption "reviewerDeleted"

instance FromJSON TopicChanged where
    parseJSON = genericParseJSON $ aesonOption "topicChanged"

instance FromJSON WorkInProgressStateChanged where
    parseJSON = genericParseJSON $ aesonOption "workInProgressStateChanged"

instance FromJSON PrivateStateChanged where
    parseJSON = genericParseJSON $ aesonOption "privateStateChanged"

instance FromJSON VoteDeleted where
    parseJSON = genericParseJSON $ aesonOption "voteDeleted"

-- | The event "type" data type
data EventType
    = AssigneeChangedEvent
    | ChangeAbandonedEvent
    | ChangeDeletedEvent
    | ChangeMergedEvent
    | ChangeRestoredEvent
    | CommentAddedEvent
    | DroppedOutputEvent
    | HashtagsChangedEvent
    | ProjectCreatedEvent
    | PatchsetCreatedEvent
    | RefUpdatedEvent
    | ReviewerAddedEvent
    | ReviewerDeletedEvent
    | TopicChangedEvent
    | WorkInProgressStateChangedEvent
    | PrivateStateChangedEvent
    | VoteDeletedEvent
    deriving (Show, Eq)

instance FromJSON EventType where
    parseJSON (String v) = case v of
        "assignee-changed" -> pure AssigneeChangedEvent
        "change-abandoned" -> pure ChangeAbandonedEvent
        "change-deleted" -> pure ChangeDeletedEvent
        "change-merged" -> pure ChangeMergedEvent
        "change-restored" -> pure ChangeRestoredEvent
        "comment-added" -> pure CommentAddedEvent
        "dropped-output" -> pure DroppedOutputEvent
        "hashtags-changed" -> pure HashtagsChangedEvent
        "project-created" -> pure ProjectCreatedEvent
        "patchset-created" -> pure PatchsetCreatedEvent
        "ref-updated" -> pure RefUpdatedEvent
        "reviewer-added" -> pure ReviewerAddedEvent
        "reviewer-deleted" -> pure ReviewerDeletedEvent
        "topic-changed" -> pure TopicChangedEvent
        "wip-state-changed" -> pure WorkInProgressStateChangedEvent
        "private-state-changed" -> pure PrivateStateChangedEvent
        "vote-deleted" -> pure VoteDeletedEvent
        _ -> mzero
    parseJSON _ = mzero

-- | The main event union data type
data Event
    = EventAssigneeChanged AssigneeChanged
    | EventChangeAbandoned ChangeAbandoned
    | EventChangeDeleted ChangeDeleted
    | EventChangeMerged ChangeMerged
    | EventChangeRestored ChangeRestored
    | EventCommentAdded CommentAdded
    | EventDroppedOutput
    | EventHashtagsChanged HashtagsChanged
    | EventProjectCreated ProjectCreated
    | EventPatchsetCreated PatchsetCreated
    | EventRefUpdated RefUpdated
    | EventReviewerAdded ReviewerAdded
    | EventReviewerDeleted ReviewerDeleted
    | EventTopicChanged TopicChanged
    | EventWorkInProgressStateChanged WorkInProgressStateChanged
    | EventPrivateStateChanged PrivateStateChanged
    | EventVoteDeleted VoteDeleted
    deriving (Show, Eq)

instance FromJSON Event where
    parseJSON o@(Object v) = do
        eType <- v .: "type"
        case eType of
            AssigneeChangedEvent -> EventAssigneeChanged <$> parseJSON o
            ChangeAbandonedEvent -> EventChangeAbandoned <$> parseJSON o
            ChangeDeletedEvent -> EventChangeDeleted <$> parseJSON o
            ChangeMergedEvent -> EventChangeMerged <$> parseJSON o
            ChangeRestoredEvent -> EventChangeRestored <$> parseJSON o
            CommentAddedEvent -> EventCommentAdded <$> parseJSON o
            DroppedOutputEvent -> pure EventDroppedOutput
            HashtagsChangedEvent -> EventHashtagsChanged <$> parseJSON o
            ProjectCreatedEvent -> EventProjectCreated <$> parseJSON o
            PatchsetCreatedEvent -> EventPatchsetCreated <$> parseJSON o
            RefUpdatedEvent -> EventRefUpdated <$> parseJSON o
            ReviewerAddedEvent -> EventReviewerAdded <$> parseJSON o
            ReviewerDeletedEvent -> EventReviewerDeleted <$> parseJSON o
            TopicChangedEvent -> EventTopicChanged <$> parseJSON o
            WorkInProgressStateChangedEvent -> EventWorkInProgressStateChanged <$> parseJSON o
            PrivateStateChangedEvent -> EventPrivateStateChanged <$> parseJSON o
            VoteDeletedEvent -> EventVoteDeleted <$> parseJSON o
    parseJSON _ = mzero

-------------------------------------------------------------------------------
-- Utilities function
-------------------------------------------------------------------------------

-- | Get the associated change
getChange :: Event -> Maybe Change
getChange event = case event of
    EventAssigneeChanged AssigneeChanged{..} -> Just assigneeChangedChange
    EventChangeAbandoned ChangeAbandoned{..} -> Just changeAbandonedChange
    EventChangeDeleted ChangeDeleted{..} -> Just changeDeletedChange
    EventChangeMerged ChangeMerged{..} -> Just changeMergedChange
    EventChangeRestored ChangeRestored{..} -> Just changeRestoredChange
    EventCommentAdded CommentAdded{..} -> Just commentAddedChange
    EventDroppedOutput -> Nothing
    EventHashtagsChanged HashtagsChanged{..} -> Just hashtagsChangedChange
    EventProjectCreated _ -> Nothing
    EventPatchsetCreated PatchsetCreated{..} -> Just patchsetCreatedChange
    EventRefUpdated _ -> Nothing
    EventReviewerAdded ReviewerAdded{..} -> Just reviewerAddedChange
    EventReviewerDeleted ReviewerDeleted{..} -> Just reviewerDeletedChange
    EventTopicChanged TopicChanged{..} -> Just topicChangedChange
    EventWorkInProgressStateChanged WorkInProgressStateChanged{..} -> Just workInProgressStateChangedChange
    EventPrivateStateChanged PrivateStateChanged{..} -> Just privateStateChangedChange
    EventVoteDeleted VoteDeleted{..} -> Just voteDeletedChange

-- | Get the associated patchset
getPatchSet :: Event -> Maybe PatchSet
getPatchSet event = case event of
    EventAssigneeChanged _ -> Nothing
    EventChangeAbandoned ChangeAbandoned{..} -> Just changeAbandonedPatchSet
    EventChangeDeleted _ -> Nothing
    EventChangeMerged ChangeMerged{..} -> Just changeMergedPatchSet
    EventChangeRestored ChangeRestored{..} -> Just changeRestoredPatchSet
    EventCommentAdded CommentAdded{..} -> Just commentAddedPatchSet
    EventDroppedOutput -> Nothing
    EventHashtagsChanged _ -> Nothing
    EventProjectCreated _ -> Nothing
    EventPatchsetCreated PatchsetCreated{..} -> Just patchsetCreatedPatchSet
    EventRefUpdated _ -> Nothing
    EventReviewerAdded ReviewerAdded{..} -> Just reviewerAddedPatchSet
    EventReviewerDeleted ReviewerDeleted{..} -> Just reviewerDeletedPatchSet
    EventTopicChanged _ -> Nothing
    EventWorkInProgressStateChanged WorkInProgressStateChanged{..} -> Just workInProgressStateChangedPatchSet
    EventPrivateStateChanged PrivateStateChanged{..} -> Just privateStateChangedPatchSet
    EventVoteDeleted VoteDeleted{..} -> Just voteDeletedPatchSet

-- | Get the associated author
getUser :: Event -> Maybe User
getUser event = case event of
    EventAssigneeChanged AssigneeChanged{..} -> Just assigneeChangedChanger
    EventChangeAbandoned ChangeAbandoned{..} -> Just changeAbandonedAbandoner
    EventChangeDeleted ChangeDeleted{..} -> Just changeDeletedDeleter
    EventChangeMerged ChangeMerged{..} -> Just changeMergedSubmitter
    EventChangeRestored ChangeRestored{..} -> Just changeRestoredRestorer
    EventCommentAdded CommentAdded{..} -> Just commentAddedAuthor
    EventDroppedOutput -> Nothing
    EventHashtagsChanged HashtagsChanged{..} -> Just hashtagsChangedEditor
    EventProjectCreated _ -> Nothing
    EventPatchsetCreated PatchsetCreated{..} -> Just patchsetCreatedUploader
    EventRefUpdated _ -> Nothing
    EventReviewerAdded ReviewerAdded{..} -> Just reviewerAddedAdder
    EventReviewerDeleted ReviewerDeleted{..} -> Just reviewerDeletedRemover
    EventTopicChanged TopicChanged{..} -> Just topicChangedChanger
    EventWorkInProgressStateChanged WorkInProgressStateChanged{..} -> Just workInProgressStateChangedChanger
    EventPrivateStateChanged PrivateStateChanged{..} -> Just privateStateChangedChanger
    EventVoteDeleted VoteDeleted{..} -> Just voteDeletedRemover

-- | Get the creation timestamp
getCreatedOn :: Event -> Maybe Int64
getCreatedOn event = case event of
    EventAssigneeChanged AssigneeChanged{..} -> Just assigneeChangedEventCreatedOn
    EventChangeAbandoned ChangeAbandoned{..} -> Just changeAbandonedEventCreatedOn
    EventChangeDeleted _ -> Nothing
    EventChangeMerged ChangeMerged{..} -> Just changeMergedEventCreatedOn
    EventChangeRestored ChangeRestored{..} -> Just changeRestoredEventCreatedOn
    EventCommentAdded CommentAdded{..} -> Just commentAddedEventCreatedOn
    EventDroppedOutput -> Nothing
    EventHashtagsChanged HashtagsChanged{..} -> Just hashtagsChangedEventCreatedOn
    EventProjectCreated ProjectCreated{..} -> Just projectCreatedEventCreatedOn
    EventPatchsetCreated PatchsetCreated{..} -> Just patchsetCreatedEventCreatedOn
    EventRefUpdated RefUpdated{..} -> Just refUpdatedEventCreatedOn
    EventReviewerAdded ReviewerAdded{..} -> Just reviewerAddedEventCreatedOn
    EventReviewerDeleted ReviewerDeleted{..} -> Just reviewerDeletedEventCreatedOn
    EventTopicChanged TopicChanged{..} -> Just topicChangedEventCreatedOn
    EventWorkInProgressStateChanged WorkInProgressStateChanged{..} -> Just workInProgressStateChangedEventCreatedOn
    EventPrivateStateChanged PrivateStateChanged{..} -> Just privateStateChangedChangedEventCreatedOn
    EventVoteDeleted _ -> Nothing

-- | Get the 'EventType' back from an 'Event'
getEventType :: Event -> EventType
getEventType event = case event of
    EventAssigneeChanged _ -> AssigneeChangedEvent
    EventChangeAbandoned _ -> ChangeAbandonedEvent
    EventChangeDeleted _ -> ChangeDeletedEvent
    EventChangeMerged _ -> ChangeMergedEvent
    EventChangeRestored _ -> ChangeRestoredEvent
    EventCommentAdded _ -> CommentAddedEvent
    EventDroppedOutput -> DroppedOutputEvent
    EventHashtagsChanged _ -> HashtagsChangedEvent
    EventProjectCreated _ -> ProjectCreatedEvent
    EventPatchsetCreated _ -> PatchsetCreatedEvent
    EventRefUpdated _ -> RefUpdatedEvent
    EventReviewerAdded _ -> ReviewerAddedEvent
    EventReviewerDeleted _ -> ReviewerDeletedEvent
    EventTopicChanged _ -> TopicChangedEvent
    EventWorkInProgressStateChanged _ -> WorkInProgressStateChangedEvent
    EventPrivateStateChanged _ -> PrivateStateChangedEvent
    EventVoteDeleted _ -> VoteDeletedEvent

-- | Get the text representation suitable for event subscription.
eventName :: EventType -> Text
eventName eventType = case eventType of
    AssigneeChangedEvent -> "assignee-changed"
    ChangeAbandonedEvent -> "change-abandoned"
    ChangeDeletedEvent -> "change-deleted"
    ChangeMergedEvent -> "change-merged"
    ChangeRestoredEvent -> "change-restored"
    CommentAddedEvent -> "comment-added"
    DroppedOutputEvent -> "dropped-output"
    HashtagsChangedEvent -> "hashtags-changed"
    ProjectCreatedEvent -> "project-created"
    PatchsetCreatedEvent -> "patchset-created"
    RefUpdatedEvent -> "ref-updated"
    ReviewerAddedEvent -> "reviewer-added"
    ReviewerDeletedEvent -> "reviewer-deleted"
    TopicChangedEvent -> "topic-changed"
    WorkInProgressStateChangedEvent -> "wip-state-changed"
    PrivateStateChangedEvent -> "private-state-changed"
    VoteDeletedEvent -> "vote-deleted"
