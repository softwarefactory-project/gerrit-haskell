{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Gerrit.Data.Change
  ( GerritQuery (..),
    GerritChangeStatus (..),
    GerritRevisionKind (..),
    GerritFile (..),
    GerritCommit (..),
    GerritRevision (..),
    GerritDetailedLabelVote (..),
    GerritDetailedLabel (..),
    GerritAuthor (..),
    GerritCommitAuthor (..),
    GerritChangeMessage (..),
    GerritChange (..),
    GerritTime (..),
    changeQS,
    queryText,
    defaultQueryChangeOptions,
    hasLabel,
  )
where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import GHC.Generics (Generic)

aesonOptions :: Options
aesonOptions = defaultOptions {fieldLabelModifier = recordToJson}
  where
    recordToJson "number" = "_number"
    recordToJson "account_id" = "_account_id"
    recordToJson "aAccountId" = "_account_id"
    recordToJson "aName" = "name"
    recordToJson "aEmail" = "email"
    recordToJson "aUsername" = "username"
    recordToJson "more_changes" = "_more_changes"
    recordToJson n = n

-- https://gerrit-review.googlesource.com/Documentation/user-search.html
data GerritQuery
  = Status GerritChangeStatus
  | Owner Text
  | CommitMessage Text
  | Project Text
  | ChangeId Text
  | After UTCTime
  deriving (Eq, Show)

-- | Convert a GerritQuery object to the search terms
queryText :: GerritQuery -> Text
queryText (Status stat) = "status:" <> T.toLower (T.pack $ show stat)
queryText (Owner owner') = "owner:" <> owner'
queryText (CommitMessage message) = "message:" <> message
queryText (Project project') = "project:" <> project'
queryText (ChangeId changeId) = "change:" <> changeId
queryText (After date') = "after:" <> T.pack formatedDate
  where
    formatedDate = formatTime defaultTimeLocale "%F" date'

defaultQueryChangeOptions :: Text
defaultQueryChangeOptions =
  "o="
    <> T.intercalate
      "&o="
      [ "MESSAGES",
        "DETAILED_ACCOUNTS",
        "DETAILED_LABELS",
        "CURRENT_REVISION",
        "CURRENT_FILES",
        "CURRENT_COMMIT"
      ]

-- | Build the Query String for the changes endpoint
-- >>> changeQS 10 [Project "software-factory/gerrit-haskell"] Nothing
-- "q=project:software-factory/gerrit-haskell&n=10&o=MESSAGES&o=DETAILED_ACCOUNTS&o=DETAILED_LABELS&o=CURRENT_REVISION&o=CURRENT_FILES&o=CURRENT_COMMIT"
-- >>> changeQS 10 [Project "software-factory/gerrit-haskell"] $ Just 100
-- "q=project:software-factory/gerrit-haskell&n=10&o=MESSAGES&o=DETAILED_ACCOUNTS&o=DETAILED_LABELS&o=CURRENT_REVISION&o=CURRENT_FILES&o=CURRENT_COMMIT&start=100"
changeQS :: Int -> [GerritQuery] -> Maybe Int -> Text
changeQS count queries startM =
  let base =
        T.intercalate
          "&"
          [ changeString,
            countString,
            defaultQueryChangeOptions
          ]
   in base <> startString
  where
    changeString = "q=" <> T.intercalate "+" (map queryText queries)
    countString = "n=" <> T.pack (show count)
    startString = maybe mempty (\s -> "&start=" <> T.pack (show s)) startM

-- | Check if a gerrit change as a label
hasLabel :: T.Text -> Int -> GerritChange -> Bool
hasLabel label labelValue change = case M.lookup label (labels change) of
  Just gerritLabel ->
    (> 0) $
      length $
        filter
          (\vote -> fromMaybe 0 (value vote) == labelValue)
          (fromMaybe [] (Gerrit.Data.Change.all gerritLabel))
  _ -> False

data GerritChangeStatus = NEW | MERGED | ABANDONED | DRAFT
  deriving (Eq, Show, Generic, FromJSON)

-- https://gerrit-review.googlesource.com/Documentation/json.html
data GerritRevisionKind = REWORK | TRIVIAL_REBASE | MERGE_FIRST_PARENT_UPDATE | NO_CODE_CHANGE | NO_CHANGE
  deriving (Eq, Show, Generic, FromJSON)

data GerritFile = GerritFile
  { gfStatus :: Maybe Text,
    gfLinesInserted :: Maybe Int,
    gfLinesDeleted :: Maybe Int,
    gfSizeDelta :: Maybe Int,
    gfSize :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON GerritFile where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data GerritCommit = GerritCommit
  { cAuthor :: GerritCommitAuthor,
    cCommitter :: GerritCommitAuthor,
    cSubject :: Text,
    cMessage :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON GerritCommit where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data GerritRevision = GerritRevision
  { grRef :: Text,
    grKind :: GerritRevisionKind,
    grFiles :: M.Map Text GerritFile,
    grCommit :: GerritCommit,
    grUploader :: GerritAuthor
  }
  deriving (Eq, Show, Generic)

instance FromJSON GerritRevision where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data GerritDetailedLabelVote = GerritDetailedLabelVote
  { value :: Maybe Int,
    account_id :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON GerritDetailedLabelVote where
  parseJSON = genericParseJSON aesonOptions

data GerritDetailedLabel = GerritDetailedLabel
  { all :: Maybe [GerritDetailedLabelVote],
    default_value :: Int
  }
  deriving (Eq, Show, Generic, FromJSON)

data GerritAuthor = GerritAuthor
  { aAccountId :: Int,
    aName :: Text,
    aEmail :: Maybe Text,
    aUsername :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON GerritAuthor where
  parseJSON = genericParseJSON aesonOptions

data GerritCommitAuthor = GerritCommitAuthor
  { caName :: Text,
    caEmail :: Maybe Text,
    caDate :: GerritTime
  }
  deriving (Eq, Show, Generic)

instance FromJSON GerritCommitAuthor where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype GerritTime = GerritTime {unGerritTime :: UTCTime}
  deriving (Eq, Show)

instance FromJSON GerritTime where
  parseJSON = withText "UTCTimePlus" (parse . T.unpack)
    where
      format = "%F %T.000000000"
      tryParse f s = parseTimeM False defaultTimeLocale f s
      parse s = GerritTime <$> tryParse format s

data GerritChangeMessage = GerritChangeMessage
  { mId :: Text,
    mAuthor :: Maybe GerritAuthor,
    mDate :: GerritTime,
    mMessage :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON GerritChangeMessage where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

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
    labels :: M.Map Text GerritDetailedLabel,
    messages :: [GerritChangeMessage],
    owner :: GerritAuthor,
    created :: GerritTime,
    updated :: GerritTime,
    submitted :: Maybe GerritTime,
    submitter :: Maybe GerritAuthor,
    insertions :: Int,
    deletions :: Int,
    more_changes :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON GerritChange where
  parseJSON = genericParseJSON aesonOptions
