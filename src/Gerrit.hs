{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: (c) 2020 software factory dev
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Software Factory Dev <softwarefactory-dev@redhat.com>
--
-- A gerrit client
module Gerrit
  ( withClient,
    getVersion,
    GerritStatus (..),
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req

data GerritClient
  = GerritClient
      { host :: Text,
        path :: Text
      }

data GerritStatus = GerritStatus Text
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

gerritGet ::
  (MonadHttp m, FromJSON b) =>
  (Url 'Https -> Url scheme) ->
  GerritClient ->
  m b
gerritGet pathCb (GerritClient {..}) =
  do
    resp <- req GET (pathCb (https host /: path)) NoReqBody lbsResponse mempty
    pure $ fromJust $ decode $ BSL.drop 5 $ responseBody resp

getVersion :: MonadHttp m => GerritClient -> m GerritStatus
getVersion = gerritGet (\x -> x /: "config" /: "server" /: "version")

withClient :: Text -> Text -> (GerritClient -> Req ()) -> IO ()
withClient host path f = runReq defaultHttpConfig $ f (GerritClient {..})
