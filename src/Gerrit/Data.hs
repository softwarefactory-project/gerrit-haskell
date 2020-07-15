{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module contains the gerrit data type
module Gerrit.Data
  ( GerritVersion (..),
    GerritQuery (..),
    GerritChangeStatus (..),
    GerritChange (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype GerritVersion = GerritVersion Text
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data GerritQuery
  = Status Text
  | Owner Text

data GerritChangeStatus = NEW | MERGED | ABANDONED | DRAFT
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

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
