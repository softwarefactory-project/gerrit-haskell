-- | This module contains the internal gerrit REST client
module Gerrit.Client
  ( GerritClient (baseUrl),
    withClient,
    gerritGet,
    gerritPost,
  )
where

import Data.Aeson (FromJSON, ToJSON, decode, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Data.Text (Text, unpack)
import qualified Data.Text.Encoding as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (lookupEnv)

data GerritClient
  = GerritClient
      { baseUrl :: Text,
        manager :: Manager,
        auth :: Maybe (Text, Text)
      }

withClient :: Text -> Maybe Text -> (GerritClient -> IO ()) -> IO ()
withClient url username callBack =
  do
    manager <- newManager tlsManagerSettings
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
        applyBasicProxyAuth (T.encodeUtf8 user) (T.encodeUtf8 pass)
          <$> parseUrlThrow (unpack $ baseUrl <> "a/" <> path)
      Nothing -> parseUrlThrow (unpack $ baseUrl <> path)
    let request = initRequest {requestBody = RequestBodyLBS $ encode postData}
    response <- httpLbs request manager
    gerritDecode response

gerritGet :: (FromJSON a) => Text -> GerritClient -> IO a
gerritGet path GerritClient {..} =
  do
    request <- parseUrlThrow (unpack $ baseUrl <> path)
    response <- httpLbs request manager
    gerritDecode response
