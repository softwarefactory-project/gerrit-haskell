{-# LANGUAGE DataKinds #-}

-- | This module contains the internal gerrit REST client
module Gerrit.Client
  ( GerritClient,
    withClient,
    gerritGet,
  )
where

import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.HTTP.Req

data GerritClient
  = GerritClient
      { host :: Text,
        path :: Maybe Text,
        url :: Url 'Https
      }

withClient :: Text -> Maybe Text -> (GerritClient -> Req ()) -> IO ()
withClient host path f = runReq defaultHttpConfig $ f (GerritClient {..})
  where
    url = case path of
      Just p -> https host /: p
      Nothing -> https host

gerritGet ::
  (MonadHttp m, FromJSON b) =>
  (Url 'Https -> Url scheme) ->
  GerritClient ->
  m b
gerritGet pathCb GerritClient {..} =
  do
    resp <- req GET (pathCb url) NoReqBody lbsResponse mempty
    pure $ fromJust $ decode $ BSL.drop 5 $ responseBody resp
