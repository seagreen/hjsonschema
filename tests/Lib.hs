{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy           as LBS
import           Data.Char                      (toLower)
import qualified Data.HashMap.Strict            as H
import           Data.JsonSchema
import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Vector                    (Vector)
import qualified Data.Vector                    as V
import           System.FilePath                ((</>))
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     hiding (Test)

isLocal :: String -> Bool
isLocal file = (file /= "definitions.json")
            && (file /= "ref.json")
            && (file /= "refRemote.json")

data SchemaTest = SchemaTest
  { _stDescription :: Text
  , _stSchema      :: RawSchema
  , _stCases       :: [SchemaTestCase]
  }

data SchemaTestCase = SchemaTestCase
  { _scDescription :: Text
  , _scData        :: Value
  , _scValid       :: Bool
  }

instance FromJSON RawSchema where
  parseJSON = withObject "Schema" $ \o ->
    return RawSchema { _rsURI = "", _rsObject = o }

instance FromJSON SchemaTest where
  parseJSON = withObject "SchemaTest" $ \o -> SchemaTest
    <$> o .: "description"
    <*> o .: "schema"
    <*> o .: "tests" -- I wish this were "cases"

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
        Right schemaTests -> return $ prependFileName name <$> schemaTests

    prependFileName :: String -> SchemaTest -> SchemaTest
    prependFileName fileName s = s
      { _stDescription = T.pack fileName <> ": " <> _stDescription s
      }

    concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
    concatMapM f xs = liftM concat (mapM f xs)

toTest :: SchemaTest -> Test
toTest st =
  testCase (T.unpack $ _stDescription st) $ do
    assertEqual "schema validity errors" V.empty $ isValidSchema (_stSchema st)
    forM_ (_stCases st) $ \sc -> do
      g <- assertRight =<< fetchRefs draft4 (_stSchema st) H.empty
      let es = validate (compile draft4 g $ _stSchema st) (_scData sc)
      if _scValid sc
        then assertValid   sc es
        else assertInvalid sc es

assertValid :: SchemaTestCase -> Vector ValErr -> Assertion
assertValid sc es =
  unless (V.length es == 0) $ assertFailure $ unlines
    [ "    Failed to validate data"
    , "    Description: " <> T.unpack (_scDescription sc)
    , "    Data: "        <> show (_scData sc)
    , "    Errors: "      <> show es
    ]

assertInvalid :: SchemaTestCase -> Vector ValErr -> Assertion
assertInvalid sc es =
  unless (V.length es > 0) $ assertFailure $ unlines
    [ "    Failed to invalidate data"
    , "    Description: " <> T.unpack (_scDescription sc)
    , "    Data: "        <> show (_scData sc)
    ]

assertRight :: (Show a) => Either a b -> IO b
assertRight a =
  case a of
    Left e  -> assertFailure (show e) >> fail "assertRight failed"
    Right b -> return b

$(deriveFromJSON defaultOptions { fieldLabelModifier = map toLower . drop 3 } ''SchemaTestCase)
