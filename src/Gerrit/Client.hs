{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the internal gerrit REST client
module Gerrit.Client
  ( GerritClient (baseUrl),
    withClient,
    gerritGet,
    gerritPost,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL (newOpenSSLManager, withOpenSSL)
import System.Environment (lookupEnv)

-- | The GerritClient record, use 'withClient' to create
data GerritClient = GerritClient
  { baseUrl :: Text,
    manager :: Manager,
    auth :: Maybe (Text, Text)
  }

-- | Create the 'GerritClient'
withClient ::
  -- | The gerrit api url
  Text ->
  -- | A username (password is read from GERRIT_PASSWORD environment)
  Maybe Text ->
  -- | The callback
  (GerritClient -> IO a) ->
  -- | withClient performs the IO
  IO a
withClient url username callBack = withOpenSSL $ do
  manager <- newOpenSSLManager
  auth <- case username of
    Just user -> do
      pass <- lookupEnv "GERRIT_PASSWORD"
      pure $ Just (user, T.pack $ fromMaybe "" pass)
    _ -> pure Nothing
  callBack (GerritClient {..})
  where
    baseUrl = T.dropWhileEnd (== '/') url <> "/"

gerritDecode :: (FromJSON a, Applicative f) => Response BSL.ByteString -> f a
gerritDecode response = case eitherDecode $ BSL.drop 5 $ responseBody response of
  Left err -> error $ "Decoding of " <> show (responseBody response) <> " failed with: " <> err
  Right a -> pure a

gerritPost :: (ToJSON a, FromJSON b) => Text -> a -> GerritClient -> IO b
gerritPost path postData GerritClient {..} =
  do
    initRequest <- case auth of
      Just (user, pass) ->
        applyBasicAuth (T.encodeUtf8 user) (T.encodeUtf8 pass)
          <$> parseUrlThrow (unpack $ baseUrl <> "a/" <> path)
      Nothing -> parseUrlThrow (unpack $ baseUrl <> path)
    let request =
          initRequest
            { method = "POST",
              requestHeaders = requestHeaders initRequest <> [("Content-Type", "application/json; charset=UTF-8")],
              requestBody = RequestBodyLBS $ encode postData
            }
    response <- httpLbs request manager
    gerritDecode response

gerritGet :: (FromJSON a) => Text -> GerritClient -> IO a
gerritGet path GerritClient {..} =
  do
    let url = baseUrl <> path
    request <- parseUrlThrow (unpack url)
    response <- httpLbs request manager
    gerritDecode response
