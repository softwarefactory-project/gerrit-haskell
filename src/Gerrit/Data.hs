{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module contains the gerrit data type
module Gerrit.Data
  ( GerritVersion (..),
    GerritQuery (..),
    GerritChangeStatus (..),
    GerritChange (..),
    GerritRevision (..),
    GerritLabel (..),
    GerritAccount (..),
    GerritLabelVote (..),
    GerritDetailedLabelVote (..),
    GerritDetailedLabel (..),
    queryText,
  )
where

import Data.Aeson
import Data.Char (toLower)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)

newtype GerritVersion = GerritVersion Text
  deriving (Show, Generic, FromJSON)

-- https://gerrit-review.googlesource.com/Documentation/json.html
data GerritRevisionKind = REWORK | TRIVIAL_REBASE | MERGE_FIRST_PARENT_UPDATE | NO_CODE_CHANGE | NO_CHANGE
  deriving (Eq, Show, Generic, FromJSON)

data GerritChangeStatus = NEW | MERGED | ABANDONED | DRAFT
  deriving (Eq, Show, Generic, FromJSON)

data GerritLabelVote = REJECTED | APPROVED | DISLIKED | RECOMMENDED
  deriving (Eq, Show, Ord, Generic)

-- We use a custom parseJSON to decode Label Vote as lowercase
instance FromJSON GerritLabelVote where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = map toLower}

instance FromJSONKey GerritLabelVote where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions {keyModifier = map toLower}

-- https://gerrit-review.googlesource.com/Documentation/user-search.html
data GerritQuery
  = Status GerritChangeStatus
  | Owner Text
  | CommitMessage Text
  | Project Text
  | ChangeId Text

queryText :: GerritQuery -> Text
queryText (Status stat) = "status:" <> T.toLower (T.pack $ show stat)
queryText (Owner owner) = "owner:" <> owner
queryText (CommitMessage message) = "message:" <> message
queryText (Project project) = "project:" <> project
queryText (ChangeId changeId) = "change:" <> changeId

data GerritRevision
  = GerritRevision
      { ref :: Text,
        kind :: GerritRevisionKind
      }
  deriving (Show, Generic, FromJSON)

newtype GerritAccount
  = GerritAccount
      { unused_account_id :: Int
      }
  deriving (Show, Generic)

-- We use a cusom parseJSON to decode `_account_id` as `account_id`
instance FromJSON GerritAccount where
  parseJSON = genericParseJSON aesonOptions

newtype GerritLabel
  = GerritLabel (M.Map GerritLabelVote GerritAccount)
  deriving (Show, Generic, FromJSON)

data GerritDetailedLabelVote
  = GerritDetailedLabelVote
      { value :: Maybe Int,
        account_id :: Int
      }
  deriving (Show, Generic)

-- We use a cusom parseJSON to decode record field properly
instance FromJSON GerritDetailedLabelVote where
  parseJSON = genericParseJSON aesonOptions

data GerritDetailedLabel
  = GerritDetailedLabel
      { all :: [GerritDetailedLabelVote],
        default_value :: Int
      }
  deriving (Show, Generic, FromJSON)

data GerritChange
  = GerritChange
      { id :: Text,
        project :: Text,
        branch :: Text,
        subject :: Text,
        status :: GerritChangeStatus,
        mergeable :: Maybe Bool,
        revisions :: M.Map Text (Maybe GerritRevision),
        number :: Int,
        labels :: M.Map Text GerritDetailedLabel
      }
  deriving (Show, Generic)

-- We use a cusom parseJSON to decode `_number` as `number`
instance FromJSON GerritChange where
  parseJSON = genericParseJSON aesonOptions

aesonOptions :: Options
aesonOptions = defaultOptions {fieldLabelModifier = recordToJson}
  where
    recordToJson "number" = "_number"
    recordToJson "account_id" = "_account_id"
    recordToJson n = n
