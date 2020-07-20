module Main (main) where

import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isJust)
import Gerrit
import Gerrit.Data
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  dataFile <- BSL.readFile "./test/data/GerritChange.json"
  withClient "http://example.com/r" $ \client ->
    defaultMain (tests dataFile client)

tests :: ByteString -> GerritClient -> TestTree
tests dataFile client = testGroup "Tests" [unitTests, encodingTests dataFile client]

unitTests :: TestTree
unitTests =
  testGroup
    "unitTests"
    [ testCase "queryString status" $
        assertEqual "Query string is valid" "status:new" (queryText (Status NEW))
    ]

encodingTests :: ByteString -> GerritClient -> TestTree
encodingTests dataFile client =
  testGroup
    "FromJSON"
    [ testCase "Test GerritChange.json"
        $ assertBool "GerritChange is decoded"
        $ isChange (decode dataFile),
      testCase
        "Test changeUrl works"
        $ assertEqual
          "Change url is correct"
          "http://example.com/r/18839"
          testGetUrl,
      testCase "Test hasLabel works" $
        assertBool "Label not found" testIsApproved
    ]
  where
    isChange :: Maybe GerritChange -> Bool
    isChange (Just _) = True
    isChange Nothing = False
    testIsApproved = case decode dataFile of
      Just change -> hasLabel "Code-Review" 1 change
      Nothing -> False
    testGetUrl = case decode dataFile of
      Just change -> changeUrl client change
      Nothing -> "decode-failed"
