module Main (main) where

import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isJust)
import Gerrit.Data
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  dataFile <- BSL.readFile "./test/data/GerritChange.json"
  defaultMain (tests dataFile)

tests :: ByteString -> TestTree
tests dataFile = testGroup "Tests" [unitTests dataFile]

unitTests :: ByteString -> TestTree
unitTests dataFile =
  testGroup
    "FromJSON"
    [ testCase "Test GerritChange.json"
        $ assertBool "GerritChange is decoded"
        $ isJust
        $ ( (decode dataFile) ::
              Maybe GerritChange
          )
    ]
