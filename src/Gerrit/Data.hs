{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module contains the gerrit data type
module Gerrit.Data
  ( GerritVersion (..),
    GerritQuery (..),
    GerritChangeStatus (..),
    GerritChange (..),
    GerritRevision (..),
    queryText,
  )
where

import Data.Aeson (FromJSON, Options (fieldLabelModifier), defaultOptions, genericParseJSON, parseJSON)
import Data.Map as M
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

data GerritChange
  = GerritChange
      { id :: Text,
        project :: Text,
        branch :: Text,
        subject :: Text,
        status :: GerritChangeStatus,
        mergeable :: Maybe Bool,
        revisions :: M.Map Text GerritRevision,
        number :: Int
      }
  deriving (Show, Generic)

-- We use a cusom parseJSON to decode `_number` as `number`
instance FromJSON GerritChange where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = recordToJson}
    where
      recordToJson "number" = "_number"
      recordToJson n = n
