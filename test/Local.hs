
module Main where

import           Control.Applicative
import           Control.Monad          (unless)
import           Data.Aeson
import           Data.List              (isSuffixOf)
import           Data.Monoid
import           System.Directory       (getDirectoryContents)
import           Test.Tasty             (TestTree, defaultMain, testGroup)
import qualified Test.Tasty.HUnit       as HU
import           Test.Tasty.QuickCheck  (testProperty)

-- Examples
import qualified CustomSchema
import qualified Full
import qualified PrettyShowFailure
import qualified Simple

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
      testGroup "Check that examples compile and don't throw errors" exampleTests
    : testGroup "QuickCheck tests" quickCheckTests
    : testGroup "Report the path to invalid data correctly" correctPaths
    : testGroup "Test the referencesViaFilesystem function" fetchFromFilesystem
    : testGroup "Test the Reference module" referenceTests
    : fmap toTest ts

quickCheckTests :: [TestTree]
quickCheckTests =
  [testProperty "Invert schemas through JSON without change" invertSchema]
  where
    invertSchema :: Schema -> Bool
    invertSchema a = Just a == decode (encode a)

exampleTests :: [TestTree]
exampleTests =
  [ HU.testCase "CustomSchema example" CustomSchema.example
  , HU.testCase "Full example" Full.example
  , HU.testCase "PrettyShowFailure example" PrettyShowFailure.example
  , HU.testCase "Simple example" Simple.example
  ]
