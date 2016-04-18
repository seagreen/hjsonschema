
module Main where

import           Control.Applicative
import           Control.Monad          (unless)
import           Data.Aeson
import           Data.List              (isSuffixOf)
import           Data.Monoid
import           System.Directory       (getDirectoryContents)
import           Test.Tasty             (defaultMain, testGroup)
import           Test.Tasty.QuickCheck  (testProperty)

import           Data.JsonSchema.Draft4
import           Local.Failure          (correctPaths)
import           Local.Filesystem       (fetchFromFilesystem)
import           Local.Reference        (referenceTests)
import           Shared                 (isLocal, readSchemaTests, toTest)

dir :: String
dir = "JSON-Schema-Test-Suite/tests/draft4"

main :: IO ()
main = do
  filenames <- filter isLocal . filter (".json" `isSuffixOf`) <$> getDirectoryContents dir
  ts <- readSchemaTests dir filenames
  defaultMain . testGroup "Tests not requiring an HTTP server" $
      testProperty "Invert schemas through JSON without change" invertSchema
    : testGroup "Report the path to invalid data correctly" correctPaths
    : testGroup "Test the referencesViaFilesystem function" fetchFromFilesystem
    : testGroup "Test the Reference module" referenceTests
    : fmap toTest ts

invertSchema :: Schema -> Bool
invertSchema a = Just a == decode (encode a)
