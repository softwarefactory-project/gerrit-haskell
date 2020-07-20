-- | The gerrit client entrypoint
module Gerrit
  ( withClient,
    getVersion,
    queryChanges,
    GerritClient,
    GerritVersion (..),
    GerritQuery (..),
    GerritChange (..),
    GerritChangeStatus (..),
    changeUrl,
    hasLabel,
    isApproved,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import Gerrit.Client
import Gerrit.Data

changeUrl :: GerritClient -> GerritChange -> T.Text
changeUrl client change = baseUrl client <> T.pack (show (number change))

getVersion :: GerritClient -> IO GerritVersion
getVersion = gerritGet "config/server/version"

queryChanges :: [GerritQuery] -> GerritClient -> IO [GerritChange]
queryChanges queries = gerritGet ("changes/?" <> queryString)
  where
    count :: Integer
    count = 2
    queryString = T.intercalate "&" [changeString, countString, option]
    changeString = "q=" <> T.intercalate "+" (map queryText queries)
    countString = "n=" <> T.pack (show count)
    option = "o=CURRENT_REVISION&o=LABELS"

hasLabel :: T.Text -> GerritLabelVote -> GerritChange -> Bool
hasLabel label value change = case M.lookup label (labels change) of
  Just (GerritLabel (Just gerritLabel)) -> case M.lookup value gerritLabel of
    Just _ -> True
    Nothing -> False
  _ -> False

isApproved :: GerritChange -> Bool
isApproved = hasLabel "Code-Review" APPROVED
