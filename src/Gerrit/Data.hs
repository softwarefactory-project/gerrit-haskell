{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

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
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data GerritChangeStatus = NEW | MERGED | ABANDONED | DRAFT
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

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
      { ref :: Text
      }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

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
  deriving stock (Show, Generic)

-- We use a cusom parseJSON to decode `_number` as `number`
instance FromJSON GerritChange where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = recordToJson}
    where
      recordToJson "number" = "_number"
      recordToJson n = n
