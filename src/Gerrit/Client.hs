{-# LANGUAGE DataKinds #-}

-- | This module contains the internal gerrit REST client
module Gerrit.Client
  ( GerritClient,
    withClient,
    gerritGet,
  )
where

import Data.Aeson (FromJSON, decode, eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

data GerritClient
  = GerritClient
      { baseUrl :: Text,
        manager :: Manager
      }

withClient :: Text -> (GerritClient -> IO ()) -> IO ()
withClient baseUrl callBack =
  do
    manager <- newManager tlsManagerSettings
    callBack (GerritClient {..})

gerritGet :: (FromJSON a) => Text -> GerritClient -> IO a
gerritGet path GerritClient {..} =
  do
    request <- parseUrlThrow (unpack $ baseUrl <> path)
    response <- httpLbs request manager
    case eitherDecode $ BSL.drop 5 $ responseBody response of
      Left err -> error $ "Decoding of " <> show (responseBody response) <> " failed with: " <> err
      Right a -> pure a