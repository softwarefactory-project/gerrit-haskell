{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module contains the gerrit data type
module Gerrit.Data
  ( GerritVersion (..),
    GerritQuery (..),
    GerritChangeStatus (..),
    GerritChange (..),
    queryText,
  )
where

import Data.Aeson (FromJSON)
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

data GerritChange
  = GerritChange
      { id :: Text,
        project :: Text,
        branch :: Text,
        subject :: Text,
        status :: GerritChangeStatus,
        mergeable :: Maybe Bool
      }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)