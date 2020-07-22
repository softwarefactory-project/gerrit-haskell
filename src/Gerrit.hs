-- | This module contains the gerrit client library
module Gerrit
  ( -- * Client
    GerritClient,
    withClient,

    -- * Api
    getVersion,
    queryChanges,
    postReview,

    -- * Main data types
    GerritVersion (..),
    GerritQuery (..),
    GerritChange (..),
    GerritChangeStatus (..),
    ReviewResult (..),

    -- * Convenient functions
    changeUrl,
    hasLabel,
  )
where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Gerrit.Client
import Gerrit.Data

-- | Return the url of a 'GerritChange'
changeUrl :: GerritClient -> GerritChange -> T.Text
changeUrl client change = baseUrl client <> T.pack (show (number change))

-- | Get the server version
getVersion :: GerritClient -> IO GerritVersion
getVersion = gerritGet "config/server/version"

-- | Search for changes
queryChanges :: [GerritQuery] -> GerritClient -> IO [GerritChange]
queryChanges queries = gerritGet ("changes/?" <> queryString)
  where
    count :: Integer
    count = 2
    queryString = T.intercalate "&" [changeString, countString, option]
    changeString = "q=" <> T.intercalate "+" (map queryText queries)
    countString = "n=" <> T.pack (show count)
    option = "o=CURRENT_REVISION&o=DETAILED_LABELS"

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
postReview change message label value client =
  do
    res <- gerritPost urlPath review client
    -- TODO: verify ReviewResult is correct
    print res
    pure res
  where
    urlPath = "changes/" <> changeId <> "/revisions/" <> revHash <> "/review"
    changeId = Gerrit.Data.id change
    revHash = fromMaybe "" (Gerrit.Data.current_revision change)
    review =
      ReviewInput
        { riMessage = Just message,
          riLabels = Just (M.fromList [(label, value)])
        }

-- | Check if a gerrit change as a label
hasLabel :: T.Text -> Int -> GerritChange -> Bool
hasLabel label labelValue change = case M.lookup label (labels change) of
  Just gerritLabel -> (> 0) $ length $ filter (\vote -> fromMaybe 0 (value vote) == labelValue) (Gerrit.Data.all gerritLabel)
  _ -> False
