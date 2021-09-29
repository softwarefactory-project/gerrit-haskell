{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust)
import Gerrit
import Gerrit.Data.Change
import qualified Gerrit.Data.Event as Event
import Gerrit.Data.Review
import System.Directory (listDirectory)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  do
    files <- listDirectory "./test/data"
    dataFiles <- traverse (BSL.readFile . ("./test/data/" <>)) files
    withClient "http://example.com/r" Nothing $ \client ->
      defaultMain (tests (zip files dataFiles) client)

tests :: [(FilePath, ByteString)] -> GerritClient -> TestTree
tests dataFiles client = testGroup "Tests" $ [unitTests] <> encodingTests dataFiles client

unitTests :: TestTree
unitTests =
  testGroup
    "unitTests"
    [ testCase "queryString status" $
        assertEqual "Query string is valid" "status:new" (queryText (Status NEW))
    ]

encodingTests :: [(FilePath, ByteString)] -> GerritClient -> [TestTree]
encodingTests dataFiles client =
  [ testGroup
      "FromJSON"
      [ testCase "Test GerritChange.json" $
          assertBool "GerritChange is decoded" $
            isChange $ decode $ getRaw "GerritChange.json",
        testCase "Test EventPatchsetCreated.json" $
          assertBool "EventPatchsetCreated is decoded" $
            isEventPatchsetCreated $ decode $ getRaw "EventPatchsetCreated.json",
        testCase "Test EventChangeMerged.json" $
          assertBool "EventChangeMerged is decoded" $
            isChangeMergedEvent $ decode $ getRaw "EventChangeMerged.json",
        testCase "Test EventCommentAdded.json" $
          assertBool "EventCommentAdded is decoded" $
            isCommentEvent $ decode $ getRaw "EventCommentAdded.json",
        testCase "Test EventChangeAbandoned.json" $
          assertBool "EventChangeAbandoned is decoded" $
            isAbandonedEvent $ decode $ getRaw "EventChangeAbandoned.json",
        testCase "Test EventProjectCreated.json" $
          assertBool "EventProjectCreated is decoded" $
            isEventProjectCreated $ decode $ getRaw "EventProjectCreated.json",
        testCase "Test EventRefUpdated.json" $
          assertBool "EventRefUpdated is decoded" $
            isEventRefUpdated $ decode $ getRaw "EventRefUpdated.json",
        testCase "Test ReviewResult.json" $
          assertBool "ReviewResult is decoded" $
            isReviewResult $ decode $ getRaw "ReviewResult.json",
        testCase "Test changeUrl works" $
          assertEqual "Change url is correct" "http://example.com/r/18839" testGetUrl,
        testCase "Test hasLabel works" $
          assertBool "Label not found" testIsApproved
      ],
    testGroup
      "toJSON"
      [ testCase "Test ReviewInput.json" $
          assertEqual "Review is encoded" (getRaw "ReviewInput.json") (prettyEncode reviewInput)
      ]
  ]
  where
    prettyEncode obj = encode obj <> "\n"
    reviewInput = ReviewInput (Just "Thanks!") Nothing
    getRaw :: FilePath -> ByteString
    getRaw fp = fromJust $ lookup fp dataFiles
    isReviewResult :: Maybe ReviewResult -> Bool
    isReviewResult = \case
      Just _ -> True
      Nothing -> False
    isChangeMergedEvent = \case
      Just (Event.EventChangeMerged _) -> True
      _ -> False
    isEventPatchsetCreated = \case
      Just (Event.EventPatchsetCreated _) -> True
      _ -> False
    isCommentEvent = \case
      Just (Event.EventCommentAdded _) -> True
      _ -> False
    isAbandonedEvent = \case
      Just (Event.EventChangeAbandoned _) -> True
      _ -> False
    isEventProjectCreated = \case
      Just (Event.EventProjectCreated _) -> True
      _ -> False
    isEventRefUpdated = \case
      Just (Event.EventRefUpdated _) -> True
      _ -> False
    isChange :: Maybe GerritChange -> Bool
    isChange = \case
      Just _ -> True
      Nothing -> False
    testIsApproved = case decode (getRaw "GerritChange.json") of
      Just change -> hasLabel "Code-Review" 2 change
      Nothing -> False
    testGetUrl = case decode (getRaw "GerritChange.json") of
      Just change -> changeUrl client change
      Nothing -> "decode-failed"
