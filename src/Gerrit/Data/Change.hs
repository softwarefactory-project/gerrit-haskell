{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Gerrit.Data.Change
  ( GerritQuery (..),
    GerritChangeStatus (..),
    GerritChange (..),
    GerritRevision (..),
    GerritDetailedLabelVote (..),
    GerritDetailedLabel (..),
    changeQS,
    queryText,
    defaultQueryChangeOptions,
    hasLabel,
  )
where

import Data.Aeson
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

aesonOptions :: Options
aesonOptions = defaultOptions {fieldLabelModifier = recordToJson}
  where
    recordToJson "number" = "_number"
    recordToJson "account_id" = "_account_id"
    recordToJson n = n

-- https://gerrit-review.googlesource.com/Documentation/user-search.html
data GerritQuery
  = Status GerritChangeStatus
  | Owner Text
  | CommitMessage Text
  | Project Text
  | ChangeId Text

-- | Convert a GerritQuery object to the search terms
queryText :: GerritQuery -> Text
queryText (Status stat) = "status:" <> T.toLower (T.pack $ show stat)
queryText (Owner owner) = "owner:" <> owner
queryText (CommitMessage message) = "message:" <> message
queryText (Project project') = "project:" <> project'
queryText (ChangeId changeId) = "change:" <> changeId

defaultQueryChangeOptions :: Text
defaultQueryChangeOptions = "o=CURRENT_REVISION&o=DETAILED_LABELS"

changeQS :: Int -> [GerritQuery] -> Text
changeQS count queries =
  T.intercalate
    "&"
    [ changeString,
      countString,
      defaultQueryChangeOptions
    ]
  where
    changeString = "q=" <> T.intercalate "+" (map queryText queries)
    countString = "n=" <> T.pack (show count)

-- | Check if a gerrit change as a label
hasLabel :: T.Text -> Int -> GerritChange -> Bool
hasLabel label labelValue change = case M.lookup label (labels change) of
  Just gerritLabel ->
    (> 0) $
      length $ filter (\vote -> fromMaybe 0 (value vote) == labelValue) (Gerrit.Data.Change.all gerritLabel)
  _ -> False

data GerritChangeStatus = NEW | MERGED | ABANDONED | DRAFT
  deriving (Eq, Show, Generic, FromJSON)

-- https://gerrit-review.googlesource.com/Documentation/json.html
data GerritRevisionKind = REWORK | TRIVIAL_REBASE | MERGE_FIRST_PARENT_UPDATE | NO_CODE_CHANGE | NO_CHANGE
  deriving (Eq, Show, Generic, FromJSON)

data GerritRevision = GerritRevision
  { ref :: Text,
    kind :: GerritRevisionKind
  }
  deriving (Show, Generic, FromJSON)

data GerritDetailedLabelVote = GerritDetailedLabelVote
  { value :: Maybe Int,
    account_id :: Int
  }
  deriving (Show, Generic)

instance FromJSON GerritDetailedLabelVote where
  parseJSON = genericParseJSON aesonOptions

data GerritDetailedLabel = GerritDetailedLabel
  { all :: [GerritDetailedLabelVote],
    default_value :: Int
  }
  deriving (Show, Generic, FromJSON)

data GerritChange = GerritChange
  { id :: Text,
    project :: Text,
    branch :: Text,
    subject :: Text,
    status :: GerritChangeStatus,
    mergeable :: Maybe Bool,
    revisions :: M.Map Text (Maybe GerritRevision),
    current_revision :: Maybe Text,
    number :: Int,
    labels :: M.Map Text GerritDetailedLabel
  }
  deriving (Show, Generic)

instance FromJSON GerritChange where
  parseJSON = genericParseJSON aesonOptions