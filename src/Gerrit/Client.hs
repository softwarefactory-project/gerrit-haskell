{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the internal gerrit REST client
module Gerrit.Client
  ( GerritClient (baseUrl),
    withClient,
    gerritGet,
    gerritPost,
    getClient,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL (newOpenSSLManager, withOpenSSL)

-- | The GerritClient record, use 'withClient' to create
data GerritClient = GerritClient
  { baseUrl :: Text,
    manager :: Manager,
    auth :: Maybe (Text, Text)
  }

-- | Need to be call through withOpenSSL
getClient :: Text -> Maybe (Text, Text) -> IO GerritClient
getClient url auth = do
  let baseUrl = T.dropWhileEnd (== '/') url <> "/"
  manager <- newOpenSSLManager
  pure $ GerritClient {..}

-- | Create the 'GerritClient'
withClient ::
  -- | The gerrit api url
  Text ->
  -- | Credentials (login, password) [Optional]
  Maybe (Text, Text) ->
  -- | The callback
  (GerritClient -> IO a) ->
  -- | withClient performs the IO
  IO a
withClient url creds callBack = withOpenSSL $ do
  client <- getClient url creds
  callBack client

gerritDecode :: (FromJSON a, Applicative f) => Response BSL.ByteString -> f a
gerritDecode response = case eitherDecode $ BSL.drop 5 $ responseBody response of
  Left err -> error $ "Decoding of " <> show (responseBody response) <> " failed with: " <> err
  Right a -> pure a

gerritRequest :: Text -> GerritClient -> IO Request
gerritRequest path GerritClient {..} =
  case auth of
    Just (user, pass) ->
      applyBasicAuth (T.encodeUtf8 user) (T.encodeUtf8 pass)
        <$> parseUrlThrow (unpack $ baseUrl <> "a/" <> path)
    Nothing -> parseUrlThrow (unpack $ baseUrl <> path)

gerritPost :: (ToJSON a, FromJSON b) => Text -> a -> GerritClient -> IO b
gerritPost path postData client@GerritClient {..} =
  do
    initRequest <- gerritRequest path client
    let request =
          initRequest
            { method = "POST",
              requestHeaders = requestHeaders initRequest <> [("Content-Type", "application/json; charset=UTF-8")],
              requestBody = RequestBodyLBS $ encode postData
            }
    response <- httpLbs request manager
    gerritDecode response

gerritGet :: (FromJSON a) => Text -> GerritClient -> IO a
gerritGet path client@GerritClient {..} =
  do
    request <- gerritRequest path client
    response <- httpLbs request manager
    gerritDecode response
