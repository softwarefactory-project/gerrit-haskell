{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the gerrit client library
module Gerrit
  ( -- * Client
    GerritClient,
    withClient,

    -- * Api
    getVersion,
    getChange,
    queryChanges,
    postReview,
    getAccountId,
    getAccount,

    -- * Main data types
    GerritVersion (..),
    GerritQuery (..),
    GerritChange (..),
    GerritChangeStatus (..),
    ReviewResult (..),
    GerritAccount (..),
    GerritAccountQuery (..),

    -- * Convenient functions
    changeUrl,
  )
where

import Control.Exception (try)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Gerrit.Client
import Gerrit.Data.Account
import Gerrit.Data.Change
import Gerrit.Data.Review
import Network.HTTP.Client (HttpException)

-- | Return the url of a 'GerritChange'
changeUrl :: GerritClient -> GerritChange -> T.Text
changeUrl client change = baseUrl client <> T.pack (show (number change))

-- | Get the server version
getVersion :: GerritClient -> IO GerritVersion
getVersion = gerritGet "config/server/version"

-- | Search for changes
queryChanges :: Int -> [GerritQuery] -> GerritClient -> IO [GerritChange]
queryChanges count queries = gerritGet ("changes/?" <> changeQS count queries)

-- | Get a change by change Id
getChange :: Int -> GerritClient -> IO (Maybe GerritChange)
getChange changeNumber client = do
  res <-
    try
      ( gerritGet ("changes/" <> T.pack (show changeNumber) <> "?" <> defaultQueryChangeOptions) client
      ) ::
      IO (Either HttpException GerritChange)
  pure $ case res of
    Right change -> Just change
    Left _err -> Nothing

-- | Post a review
postReview ::
  -- | The change to review
  GerritChange ->
  -- | A message
  Text ->
  -- | A label
  Text ->
  -- | A vote
  Int ->
  -- | The client
  GerritClient ->
  -- | Returns the ReviewResult
  IO ReviewResult
postReview change message label value' = gerritPost urlPath review
  where
    urlPath = "changes/" <> changeId <> "/revisions/" <> revHash <> "/review"
    changeId = Gerrit.Data.Change.id change
    revHash = fromMaybe "" (Gerrit.Data.Change.current_revision change)
    review =
      ReviewInput
        { riMessage = Just message,
          riLabels = Just (M.fromList [(label, value')])
        }

-- | Get user account id
getAccountId :: Int -> NonEmpty GerritAccountQuery -> GerritClient -> IO [GerritAccountId]
getAccountId count queries = gerritGet ("accounts/?" <> accountQs count queries)

-- | Get user account details
getAccount :: Int -> NonEmpty GerritAccountQuery -> GerritClient -> IO [GerritAccount]
getAccount count queries = gerritGet ("accounts/?" <> accountQs count queries <> "&o=DETAILS")
