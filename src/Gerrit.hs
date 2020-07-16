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
  )
where

import qualified Data.Text as T
import Gerrit.Client
import Gerrit.Data

getVersion :: GerritClient -> IO GerritVersion
getVersion = gerritGet "config/server/version"

queryChanges :: [GerritQuery] -> GerritClient -> IO [GerritChange]
queryChanges queries = gerritGet ("changes/?" <> queryString)
  where
    count :: Integer
    count = 2
    queryString = T.intercalate "&" [changeString, countString]
    changeString = "q=" <> T.intercalate "+" (map queryText queries)
    countString = "n=" <> T.pack (show count)
