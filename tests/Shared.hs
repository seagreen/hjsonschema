{-# LANGUAGE DeriveGeneric #-}

module Shared where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy   as LBS
import           Data.Char              (toLower)
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import           GHC.Generics
import           System.FilePath        ((</>))
import           Test.Tasty             (TestTree)
import qualified Test.Tasty.HUnit       as HU

import qualified Data.JsonSchema.Draft4 as D4

isLocal :: String -> Bool
isLocal file = (file /= "definitions.json")
            && (file /= "ref.json")
            && (file /= "refRemote.json")

data SchemaTest = SchemaTest
  { _stDescription :: Text
  , _stSchema      :: D4.Schema
  , _stCases       :: [SchemaTestCase]
  }

data SchemaTestCase = SchemaTestCase
  { _scDescription :: Text
  , _scData        :: Value
  , _scValid       :: Bool
  } deriving Generic

instance FromJSON SchemaTest where
  parseJSON = withObject "SchemaTest" $ \o -> SchemaTest
    <$> o .: "description"
    <*> o .: "schema"
    <*> o .: "tests" -- Perhaps "cases" would have been a more descriptive key.

instance FromJSON SchemaTestCase where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = fmap toLower . drop 3 }

readSchemaTests :: String -> [String] -> IO [SchemaTest]
readSchemaTests dir jsonFiles = concatMapM fileToCases jsonFiles
  where
    -- Each file contains an array of SchemaTests, not just one.
    fileToCases :: String -> IO [SchemaTest]
    fileToCases name = do
      let fullPath = dir </> name
      jsonBS <- LBS.readFile fullPath
      case eitherDecode jsonBS of
        Left e -> fail $ "couldn't parse file '" <> fullPath <> "': " <> e
        Right schemaTests -> pure $ prependFileName name <$> schemaTests

    prependFileName :: String -> SchemaTest -> SchemaTest
    prependFileName fileName s = s
      { _stDescription = T.pack fileName <> ": " <> _stDescription s
      }

    concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
    concatMapM f xs = liftM concat (mapM f xs)

toTest :: SchemaTest -> TestTree
toTest st =
  HU.testCase (T.unpack (_stDescription st)) $ do
    forM_ (_stCases st) $ \sc -> do
      g <- assertRight =<< D4.referencesViaHTTP (D4.SchemaWithURI (_stSchema st) Nothing)
      validate <- assertRight . D4.checkSchema g $ D4.SchemaWithURI (_stSchema st) Nothing
      let res = validate (_scData sc)
      if _scValid sc
        then assertValid   sc res
        else assertInvalid sc res

assertValid :: SchemaTestCase -> [D4.Failure] -> HU.Assertion
assertValid _ [] = pure ()
assertValid sc errs =
  HU.assertFailure $ unlines
    [ "    Failed to validate data"
    , "    Description: "         <> T.unpack (_scDescription sc)
    , "    Data: "                <> show (_scData sc)
    , "    Validation failures: " <> show errs
    ]

assertInvalid :: SchemaTestCase -> [D4.Failure] -> HU.Assertion
assertInvalid sc [] =
  HU.assertFailure $ unlines
    [ "    Validated invalid data"
    , "    Description: " <> T.unpack (_scDescription sc)
    , "    Data: "        <> show (_scData sc)
    ]
assertInvalid _ _ = pure ()

assertRight :: (Show a) => Either a b -> IO b
assertRight (Left e)  = HU.assertFailure (show e) >> fail "assertRight failed"
assertRight (Right b) = pure b
