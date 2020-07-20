module Main (main) where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust, isJust)
import Gerrit
import Gerrit.Data as Gerrit
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  do
    dataFiles <- traverse (BSL.readFile . ("./test/data/" <>)) files
    withClient "http://example.com/r" Nothing $ \client ->
      defaultMain (tests (zip files dataFiles) client)
  where
    files = ["GerritChange.json", "ReviewResult.json", "ReviewInput.json"]

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
    isChange :: Maybe GerritChange -> Bool
    isChange (Just _) = True
    isChange Nothing = False
    testIsApproved = case decode changeRaw of
      Just change -> hasLabel "Code-Review" 1 change
      Nothing -> False
    testGetUrl = case decode changeRaw of
      Just change -> changeUrl client change
      Nothing -> "decode-failed"
