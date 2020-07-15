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
import Network.HTTP.Req ((/:), MonadHttp)

getVersion :: MonadHttp m => GerritClient -> m GerritVersion
getVersion = gerritGet (\x -> x /: "config" /: "server" /: "version")

queryChanges :: MonadHttp m => [GerritQuery] -> GerritClient -> m [GerritChange]
queryChanges _queries = gerritGet (\x -> x /: "changes" /: "")
