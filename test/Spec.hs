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
        $ isJust
          ( decode dataFile ::
              Maybe GerritChange
          ),
      testCase "Test changeUrl works" $
        ( assertEqual
            "Change url is correct"
            "http://example.com/r/18839"
            testGetUrl
        )
    ]
  where
    testGetUrl = case decode dataFile of
      Just change -> changeUrl client change
      Nothing -> "decode-failed"
