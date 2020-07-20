module Main (main) where

import Gerrit (getVersion, withClient)

main :: IO ()
main = withClient "https://softwarefactory-project.io/r/" Nothing $ \client -> do
  version <- getVersion client
  print version