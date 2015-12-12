
module Main where

import           Control.Applicative
import           Control.Monad          (unless)
import           Data.Aeson
import qualified Data.Aeson.Pointer     as P
import           Data.List              (isSuffixOf)
import           Data.Monoid
import           System.Directory       (getDirectoryContents)
import           Test.Tasty             (TestTree, defaultMain, testGroup)
import qualified Test.Tasty.HUnit       as HU
import           Test.Tasty.QuickCheck  (testProperty)

import           Data.JsonSchema.Draft4 (Failure(..), Schema(..),
                                         SchemaCache(..), SchemaContext(..),
                                         emptySchema, runValidate)
import qualified Data.Validator.Draft4  as VA
import           Shared                 (isLocal, readSchemaTests, toTest)

dir :: String
dir = "JSON-Schema-Test-Suite/tests/draft4"

main :: IO ()
main = do
  filenames <- filter isLocal . filter (".json" `isSuffixOf`) <$> getDirectoryContents dir
  ts <- readSchemaTests dir filenames
  defaultMain . testGroup "Tests not requiring an HTTP server" $
      testProperty "Invert schemas through JSON without change" invertSchema
    : testGroup "Make paths to invalid data correctly" correctPaths
    : fmap toTest ts

invertSchema :: Schema -> Bool
invertSchema a = Just a == decode (encode a)

correctPaths :: [TestTree]
correctPaths =
  [ HU.testCase "Items object" itemsObject
  , HU.testCase "Items array" itemsArray
  ]

itemsObject :: IO ()
itemsObject = HU.assertEqual "Path to invalid data"
                             (P.Pointer [P.Token "0"])
                             (_failureOffendingData failure)
  where
    [failure] = runValidate (SchemaCache schema mempty) sc (toJSON [[True, True]])

    schema :: Schema
    schema = emptySchema
      { _schemaItems = Just (VA.ItemsObject (emptySchema { _schemaUniqueItems = Just True }))
      }

    sc :: SchemaContext Schema
    sc = SchemaContext
      { _scURI    = Nothing
      , _scSchema = schema
      }

itemsArray :: IO ()
itemsArray = HU.assertEqual "Path to invalid data"
                            (P.Pointer [P.Token "0"])
                            (_failureOffendingData failure)
  where
    [failure] = runValidate (SchemaCache schema mempty) sc (toJSON [[True, True]])

    schema :: Schema
    schema = emptySchema
      { _schemaItems = Just (VA.ItemsArray [emptySchema { _schemaUniqueItems = Just True }])
      }

    sc :: SchemaContext Schema
    sc = SchemaContext
      { _scURI    = Nothing
      , _scSchema = schema
      }
