{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the gerrit data type
module Gerrit.Data.Review (
    GerritVersion (..),
    GerritLabel (..),
    GerritReviewAccount (..),
    GerritLabelVote (..),
    ReviewResult (..),
    ReviewInput (..),
)
where

import Data.Aeson
import Data.Char (isUpper, toLower)
import qualified Data.Map as M
import Data.Text (Text)
import GHC.Generics (Generic)

newtype GerritVersion = GerritVersion Text
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data GerritLabelVote = REJECTED | APPROVED | DISLIKED | RECOMMENDED
    deriving (Eq, Show, Ord, Generic)

-- We use a custom parseJSON to decode Label Vote as lowercase
instance FromJSON GerritLabelVote where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = map toLower}

instance FromJSONKey GerritLabelVote where
    fromJSONKey = genericFromJSONKey defaultJSONKeyOptions{keyModifier = map toLower}

{- | Modify record attribute to match json schema
Remove the prefix and use snakecase
-}
customParseJSON :: String -> Options
customParseJSON prefix = defaultOptions{fieldLabelModifier = recordToJson}
  where
    recordToJson = updateCase . drop (length prefix)
    updateCase [] = []
    updateCase (x : xs) = toLower x : updateCase' xs
    updateCase' [] = []
    updateCase' (x : xs)
        | isUpper x = '_' : toLower x : updateCase' xs
        | otherwise = x : updateCase' xs

-- https://gerrit-review.googlesource.com/Documentation/rest-api-changes.html
data ReviewResult = ReviewResult
    { rrLabels :: Maybe (M.Map Text Int)
    , rrReady :: Maybe Int
    }
    deriving (Eq, Show, Ord, Generic)

instance FromJSON ReviewResult where
    parseJSON = genericParseJSON $ customParseJSON "rr"

instance ToJSON ReviewResult where
    toJSON = genericToJSON $ customParseJSON "rr"

data ReviewInput = ReviewInput
    { riMessage :: Maybe Text
    , riLabels :: Maybe (M.Map Text Int)
    }
    deriving (Eq, Show, Ord, Generic)

instance FromJSON ReviewInput where
    parseJSON = genericParseJSON $ customParseJSON "ri"

instance ToJSON ReviewInput where
    toJSON = genericToJSON $ (customParseJSON "ri"){omitNothingFields = True}

newtype GerritReviewAccount = GerritReviewAccount
    { unused_account_id :: Int
    }
    deriving (Show, Generic)

-- We use a cusom parseJSON to decode `_account_id` as `account_id`
instance FromJSON GerritReviewAccount where
    parseJSON = genericParseJSON defaultOptions

newtype GerritLabel
    = GerritLabel (M.Map GerritLabelVote GerritReviewAccount)
    deriving (Show, Generic)
    deriving anyclass (FromJSON)
