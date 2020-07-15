module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Gerrit (getVersion, withClient)

main :: IO ()
main = withClient "softwarefactory-project.io" "r" $ \client -> do
  version <- getVersion client
  liftIO $ putStrLn $ show $ version
