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

data GerritQuery
  = Status GerritChangeStatus
  | Owner Text

queryText :: GerritQuery -> Text
queryText (Status stat) = "status:" <> (T.toLower $ T.pack $ show stat)
queryText (Owner owner) = "owner:" <> owner

data GerritChange
  = GerritChange
      { id :: Text,
        project :: Text,
        branch :: Text,
        subject :: Text,
        status :: GerritChangeStatus,
        mergeable :: Bool
      }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)
