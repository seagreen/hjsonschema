module Shared where

import           Protolude

import           Control.Monad (fail)
import           Data.Aeson
import           Data.Aeson.TH (fieldLabelModifier)
import qualified Data.ByteString as BS
import           Data.Char (toLower)
import           Data.List (stripPrefix, unlines)
import qualified Data.Text as T
import qualified System.Directory as SD
import           System.FilePath ((</>))
import           Test.Hspec

skipTest :: FilePath -> Bool
skipTest file = (file == "optional/format.json") -- Optional
             || (file == "optional/zeroTerminatedFloats.json")
             || (file == "optional/ecmascript-regex.json")


isHTTPTest :: FilePath -> Bool
isHTTPTest file = (file == "definitions.json")
               || (file == "ref.json")
               || (file == "refRemote.json")

-- Recursively return the contents of a directory
-- (or return itself if given a file as an argument).
--
-- Return paths are relative to that directory.
listFiles :: FilePath -> IO [FilePath]
listFiles path = fmap ( fromMaybe "stripPrefix failed"
                      . stripPrefix (path <> "/")
                      ) <$> listFilesFullPath path

-- Recursively return the contents of a directory
-- (or return itself if given a file as an argument).
--
-- All return paths start with the 'FilePath' argument.
listFilesFullPath :: FilePath -> IO [FilePath]
listFilesFullPath path = do
    -- Check if it's a file or directory:
    res <- SD.doesFileExist path
    if res
        then pure [path]
        else do
            fs <- fmap (path </>) <$> SD.listDirectory path
            concat <$> traverse listFilesFullPath fs

data SchemaTest = SchemaTest
    { _stDescription :: Text
    , _stSchema      :: Value
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
        <*> o .: "tests" -- Perhaps "cases" would have been
                         -- a more descriptive key.

instance FromJSON SchemaTestCase where
    parseJSON = genericParseJSON
                    defaultOptions
                    { fieldLabelModifier = fmap toLower . drop 3 }

readSchemaTests
    :: FilePath
    -- ^ The path to a directory.
    -> (FilePath -> Bool)
    -- ^ A function to decide what we're interested in within that directory.
    -> IO [SchemaTest]
readSchemaTests dir filterFunc = do
    files <- filter filterFunc <$> listFiles dir
    concat <$> traverse fileToCases files
  where
    -- Each file contains an array of SchemaTests, not just one.
    fileToCases :: FilePath -> IO [SchemaTest]
    fileToCases name = do
        let fullPath = dir </> name
        jsonBS <- BS.readFile fullPath
        case eitherDecodeStrict jsonBS of
            Left e -> fail $ "couldn't parse file '" <> fullPath <> "': " <> e
            Right schemaTests -> pure $ prependFileName name <$> schemaTests

    prependFileName :: FilePath -> SchemaTest -> SchemaTest
    prependFileName fileName s = s
        { _stDescription = T.pack fileName <> ": " <> _stDescription s
        }

toTest
  :: forall schema. FromJSON schema
  => (schema -> SchemaTestCase -> Expectation)
  -> SchemaTest
  -> Spec
toTest validate st =
    it (T.unpack (_stDescription st)) $
        forM_ (_stCases st) (validate schema)
  where
    schema :: schema
    schema = case fromJSON (_stSchema st) of
                 Error e   -> panic ("Couldn't parse schema: " <> show e)
                 Success a -> a

assertResult :: Show err => SchemaTestCase -> [err] -> Expectation
assertResult sc failures
    | _scValid sc = assertValid sc failures
    | otherwise   = assertInvalid sc failures

assertValid :: Show err => SchemaTestCase -> [err] -> Expectation
assertValid _ [] = pure ()
assertValid sc failures =
    expectationFailure $ unlines
        [ "    Failed to validate data"
        , "    Description: "         <> T.unpack (_scDescription sc)
        , "    Data: "                <> show (_scData sc)
        , "    Validation failures: " <> show failures
        ]

assertInvalid :: SchemaTestCase -> [err] -> Expectation
assertInvalid sc [] =
    expectationFailure $ unlines
        [ "    Validated invalid data"
        , "    Description: " <> T.unpack (_scDescription sc)
        , "    Data: "        <> show (_scData sc)
        ]
assertInvalid _ _ = pure ()
