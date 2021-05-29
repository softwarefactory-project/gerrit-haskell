{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust)
import Gerrit
import Gerrit.Data as Gerrit
import qualified Gerrit.Event as Event
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
        testCase "Test ChangeEvent.json" $
          assertBool "ChangeEvent is decoded" $
            isChangeEvent $ decode $ getRaw "ChangeEvent.json",
        testCase "Test ChangeEventComment.json" $
          assertBool "ChangeEvent is decoded" $
            isCommentEvent $ decode $ getRaw "ChangeEventComment.json",
        testCase "Test ChangeEventAbandon.json" $
          assertBool "ChangeEvent is decoded" $
            isAbandonedEvent $ decode $ getRaw "ChangeEventAbandon.json",
        testCase "Test ProjectEvent.json" $
          assertBool "ProjectEvent is decoded" $
            isProjectEvent $ decode $ getRaw "ProjectEvent.json",
        testCase "Test RefEvent.json" $
          assertBool "RefEvent is decoded" $
            isRefEvent $ decode $ getRaw "RefEvent.json",
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
    reviewInput = Gerrit.ReviewInput (Just "Thanks!") Nothing
    getRaw :: FilePath -> ByteString
    getRaw fp = fromJust $ lookup fp dataFiles
    isReviewResult :: Maybe ReviewResult -> Bool
    isReviewResult = \case
      Just _ -> True
      Nothing -> False
    isChangeEvent :: Maybe Event.Event -> Bool
    isChangeEvent = \case
      Just (Event.EventChange _) -> True
      _ -> False
    isCommentEvent :: Maybe Event.Event -> Bool
    isCommentEvent = \case
      Just (Event.EventComment _) -> True
      _ -> False
    isAbandonedEvent :: Maybe Event.Event -> Bool
    isAbandonedEvent = \case
      Just (Event.EventAbandon _) -> True
      _ -> False
    isProjectEvent = \case
      Just (Event.EventProject _) -> True
      _ -> False
    isRefEvent :: Maybe Event.Event -> Bool
    isRefEvent = \case
      Just (Event.EventRef _) -> True
      _ -> False
    isChange :: Maybe GerritChange -> Bool
    isChange = \case
      Just _ -> True
      Nothing -> False
    testIsApproved = case decode (getRaw "GerritChange.json") of
      Just change -> hasLabel "Code-Review" 1 change
      Nothing -> False
    testGetUrl = case decode (getRaw "GerritChange.json") of
      Just change -> changeUrl client change
      Nothing -> "decode-failed"
