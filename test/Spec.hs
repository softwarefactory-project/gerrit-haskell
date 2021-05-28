module Main (main) where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust, isJust)
import Gerrit
import Gerrit.Data as Gerrit
import qualified Gerrit.Event as Event
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  do
    dataFiles <- traverse (BSL.readFile . ("./test/data/" <>)) files
    withClient "http://example.com/r" Nothing $ \client ->
      defaultMain (tests (zip files dataFiles) client)
  where
    files = [
      "ChangeEvent.json",
      "ChangeEventComment.json",
      "RefEvent.json",
      "ProjectEvent.json",
      "GerritChange.json",
      "ReviewResult.json",
      "ReviewInput.json"
     ]

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
      [ testCase "Test GerritChange.json"
          $ assertBool "GerritChange is decoded"
          $ isChange (decode changeRaw),
        testCase "Test ChangeEvent.json"
          $ assertBool "ChangeEvent is decoded"
          $ isChangeEvent (decode $ getRaw "ChangeEvent.json"),
        testCase "Test ChangeEventComment.json"
          $ assertBool "ChangeEvent is decoded"
          $ isCommentEvent (decode $ getRaw "ChangeEventComment.json"),
        testCase "Test ProjectEvent.json"
          $ assertBool "ProjectEvent is decoded"
          $ isProjectEvent (decode $ getRaw "ProjectEvent.json"),
        testCase "Test RefEvent.json"
          $ assertBool "RefEvent is decoded"
          $ isRefEvent (decode $ getRaw "RefEvent.json"),
        testCase "Test ReviewResult.json"
          $ assertBool "ReviewResult is decoded"
          $ isReviewResult (decode reviewResultRaw),
        testCase
          "Test changeUrl works"
          $ assertEqual
            "Change url is correct"
            "http://example.com/r/18839"
            testGetUrl,
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
    changeRaw = getRaw "GerritChange.json"
    reviewResultRaw = getRaw "ReviewResult.json"
    getRaw :: FilePath -> ByteString
    getRaw fp = fromJust $ lookup fp dataFiles
    isReviewResult :: Maybe ReviewResult -> Bool
    isReviewResult (Just _) = True
    isReviewResult Nothing = False
    isChangeEvent :: Maybe Event.Event -> Bool
    isChangeEvent (Just (Event.EventChange _)) = True
    isChangeEvent _ = False
    isCommentEvent :: Maybe Event.Event -> Bool
    isCommentEvent (Just (Event.EventComment _)) = True
    isCommentEvent _ = False
    isProjectEvent (Just (Event.EventProject _)) = True
    isProjectEvent _ = False
    isRefEvent :: Maybe Event.Event -> Bool
    isRefEvent (Just (Event.EventRef _)) = True
    isRefEvent _ = False
    isChange :: Maybe GerritChange -> Bool
    isChange (Just _) = True
    isChange Nothing = False
    testIsApproved = case decode changeRaw of
      Just change -> hasLabel "Code-Review" 1 change
      Nothing -> False
    testGetUrl = case decode changeRaw of
      Just change -> changeUrl client change
      Nothing -> "decode-failed"
