module Main where

import           Control.Applicative
import           Control.Concurrent.Async       (withAsync)
import           Data.List                      (isSuffixOf)
import           Network.Wai.Application.Static (defaultFileServerSettings,
                                                 staticApp)
import           Network.Wai.Handler.Warp       (run)
import           System.Directory               (getDirectoryContents)
import           Test.Tasty                     (defaultMain, testGroup)

import           Shared                         (isLocal, readSchemaTests,
                                                 toTest)

dir :: String
dir = "JSON-Schema-Test-Suite/tests/draft4"

main :: IO ()
main = withAsync serve $ \_ -> do
  filenames <- filter (not . isLocal) . filter (".json" `isSuffixOf`)
                 <$> getDirectoryContents dir
  ts <- readSchemaTests dir filenames
  defaultMain . testGroup "Tests that run an HTTP server" $ toTest <$> ts

serve :: IO ()
serve = run 1234 . staticApp . defaultFileServerSettings $ "JSON-Schema-Test-Suite/remotes"
