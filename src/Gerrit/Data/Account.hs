{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Gerrit.Data.Account
  ( GerritAccountId (..),
    GerritAccount (..),
    GerritAccountQuery (..),
    userQueryText,
  )
where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

-- https://gerrit-review.googlesource.com/Documentation/user-search-accounts.html#_search_operators
data GerritAccountQuery
  = CanSee Text
  | Email Text
  | Name Text
  | Username Text
  | IsActive
  | IsInactive

userQueryText :: GerritAccountQuery -> Text
userQueryText guq = case guq of
  CanSee change -> "cansee:" <> change
  Email email -> "email:" <> email
  Name name -> "name:" <> escapeChar name
  Username username -> "username:" <> username
  IsActive -> "is:active"
  IsInactive -> "is:inactive"
  where
    escapeChar = T.replace "'" " "

data GerritAccountId = GerritAccountId
  { gerritAccountId' :: Int,
    gerritAccountHasMore' :: Maybe Bool
  }
  deriving (Eq, Show)

instance FromJSON GerritAccountId where
  parseJSON (Object v) = GerritAccountId <$> v .: "_account_id" <*> v .:? "_more_accounts"
  parseJSON _ = mzero

data GerritAccount = GerritAccount
  { gerritAccountId :: Int,
    gerritAccountName :: Text,
    gerritAccountUsername :: Maybe Text,
    gerritAccountEmail :: Maybe Text,
    gerritAccountHasMore :: Maybe Bool
  }
  deriving (Eq, Show)

instance FromJSON GerritAccount where
  parseJSON (Object v) =
    GerritAccount
      <$> v .: "_account_id"
      <*> v .: "name"
      <*> v .:? "username"
      <*> v .:? "email"
      <*> v .:? "_more_accounts"
  parseJSON _ = mzero