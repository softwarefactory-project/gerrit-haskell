{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Gerrit.Data.Project (
    GerritProjectInfo (..),
    GerritProjectQuery (..),
    projectQS,
    GerritProjectsMessage,
)
where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Map
import Data.Text (Text, intercalate, pack)
import GHC.Generics (Generic)

data GerritProjectQuery = Regexp Text | Prefix Text
    deriving (Eq, Show)

queryText :: GerritProjectQuery -> Text
queryText (Regexp re) = "r=" <> re
queryText (Prefix prefix) = "p=" <> prefix

-- >>> projectQS 10 (Regexp "test/.*|rpms/.*") Nothing
-- "r:test/.*|rpms/.*&n=10"
projectQS :: Int -> GerritProjectQuery -> Maybe Int -> Text
projectQS count query startM =
    intercalate "&" [qtString, countString] <> startString
  where
    qtString = queryText query
    countString = "n=" <> pack (show count)
    startString = maybe mempty (\s -> "&S=" <> pack (show s)) startM

newtype GerritProjectInfo = GerritProjectInfo
    {gerritprojectinfoId :: Text}
    deriving (Eq, Show, Generic)

type GerritProjectsMessage = Map Text GerritProjectInfo

instance FromJSON GerritProjectInfo where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
