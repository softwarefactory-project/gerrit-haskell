-- | The gerrit client entrypoint
module Gerrit
  ( withClient,
    getVersion,
    queryChanges,
    GerritVersion (..),
    GerritQuery (..),
    GerritChange (..),
  )
where

import Gerrit.Client
import Gerrit.Data

getVersion :: GerritClient -> IO GerritVersion
getVersion = gerritGet "config/server/version"

queryChanges :: [GerritQuery] -> GerritClient -> IO [GerritChange]
queryChanges queries = gerritGet ("changes/?q=" <> queries' <> "&n=2") -- <> show count)
  where
    count :: Integer
    count = 2
    queries' = foldr (\gerritQuery acc -> toParam gerritQuery <> "+" <> acc) mempty queries
    toParam (Status stat) = "status:" <> statusToParam stat
    toParam (Owner owner) = "owner:" <> owner
    statusToParam NEW = "new"
    statusToParam MERGED = "merged"
