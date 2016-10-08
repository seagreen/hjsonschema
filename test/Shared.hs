{-# LANGUAGE DeriveGeneric #-}

module Shared where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Pointer     as AP
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy   as LBS
import           Data.Char              (toLower)
import qualified Data.HashMap.Strict    as HM
import           Data.List              (isInfixOf, stripPrefix)
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Traversable
import qualified Data.Vector            as V
import           GHC.Generics
import qualified System.Directory       as SD
import           System.FilePath        ((</>))
import           Test.Hspec

import qualified Data.JsonSchema.Draft4 as D4

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

checkPointer :: Value -> D4.Failure -> Expectation
checkPointer v failure =
    case AP.resolve (D4._failureOffendingPointer failure) v of
        Left e  -> error ("Couldn't resolve pointer: " <> show e)
        Right a -> assertContains a (D4._failureOffendingData failure)
  where
    -- Some validators, such as 'additionalItems', only return a subset
    -- of the data incated by their '_failureOffendingPointer'.
    -- See the comments on 'Data.Validator.Failure.Failure' for more info.
    assertContains :: Value -> Value -> Expectation
    assertContains x y
        | x == y    = pure ()
        | otherwise =
            case (x,y) of
                (Array xs, Array ys) ->
                    V.toList ys `shouldSatisfy` (`isInfixOf` V.toList xs)
                (Object xhm, Object yhm) ->
                    HM.toList yhm `shouldSatisfy` (`isInfixOf` HM.toList xhm)
                _ -> expectationFailure
                        "Pointer resolution incorrect: result mismatch"

isHTTPTest :: String -> Bool
isHTTPTest file = (file == "definitions.json")
               || (file == "ref.json")
               || (file == "refRemote.json")

-- | We may never support the @"format"@ keywords, and
-- are currently failing the zeroTerminatedFloats test.
skipOptional :: String -> Bool
skipOptional file = (file == "optional/format.json")
                 || (file == "optional/zeroTerminatedFloats.json")

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
    :: String
    -- ^ The path to a directory.
    -> (String -> Bool)
    -- ^ A function to decide what we're interested in within that directory.
    -> IO [SchemaTest]
readSchemaTests dir filterFunc = do
    files <- filter filterFunc <$> listFiles dir
    concat <$> traverse fileToCases files
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
                 Error e   -> error ("Couldn't parse schema: " <> show e)
                 Success a -> a

assertResult :: SchemaTestCase -> [D4.Failure] -> Expectation
assertResult sc failures
    | _scValid sc = assertValid sc failures
    | otherwise   = assertInvalid sc failures

assertValid :: SchemaTestCase -> [D4.Failure] -> Expectation
assertValid _ [] = pure ()
assertValid sc failures =
    expectationFailure $ unlines
        [ "    Failed to validate data"
        , "    Description: "         <> T.unpack (_scDescription sc)
        , "    Data: "                <> show (_scData sc)
        , "    Validation failures: " <> show failures
        ]

assertInvalid :: SchemaTestCase -> [D4.Failure] -> Expectation
assertInvalid sc [] =
    expectationFailure $ unlines
        [ "    Validated invalid data"
        , "    Description: " <> T.unpack (_scDescription sc)
        , "    Data: "        <> show (_scData sc)
        ]
assertInvalid _ _ = pure ()
