
module Local.Validation where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Aeson             as AE
import qualified Data.HashMap.Strict    as HM
import           Data.Monoid
import           Data.Text              (Text)
import           Test.Tasty             (TestTree)
import qualified Test.Tasty.HUnit       as HU

import           Data.JsonSchema.Draft4
import qualified Data.JsonSchema.Types  as JT

import qualified AlternateSchema        as AS

fetchFromFilesystem :: [TestTree]
fetchFromFilesystem =
    [ HU.testCase
        "readFile exceptions are turned into Lefts"
        readFileExceptions

    , HU.testCase
        "Relative reference to local file"
        (resolve "test/Local/schema.json")
    , HU.testCase
        "Chained relative references to local files"
        (resolve "./test/Local/schema-with-ref.json")
    ]

readFileExceptions :: IO ()
readFileExceptions = do
    res <- referencesViaFilesystem (SchemaWithURI schema Nothing)
    case res of
        Left (FSReadFailure _) -> pure ()
        a                      -> error (msg <> show a)
  where
    schema :: Schema
    schema = emptySchema { _schemaRef = Just "does-not-exist.json" }

    msg :: String
    msg = "expected referencesViaFilesystem to return ReadFailure,"
       <> " instead got: "

resolve :: Text -> IO ()
resolve ref = do
    let schema = emptySchema { _schemaRef = Just ref }
    res <- fetchFilesystemAndValidate (SchemaWithURI schema Nothing) badData
    case res of
        Left (FVData _) -> pure ()
        a               -> error (msg <> show a)
  where
    badData :: Value
    badData = toJSON [True, True]

    msg :: String
    msg = "expected fetchFilesystemAndValidate to return"
       <> " Left (FVData [_]), instead got: "

-- | Validation tests that were hard to write in JSON for whatever reason.
generalValidation :: [TestTree]
generalValidation =
    [ HU.testCase
        "Don't parse schemas that have Null in a forbidden location"
        forbidNull
    , HU.testCase
        "The 'Value' based Schema's checkSchema should catch Nulls"
        exampleForbidNull
    ]

forbidNull :: IO ()
forbidNull =
    case fromJSON (Object (HM.singleton "type" Null)) of
        AE.Error _   -> pure ()
        AE.Success a -> HU.assertFailure ("parsed to: " <> show (a :: Schema))

exampleForbidNull :: IO ()
exampleForbidNull =
    case AS.checkSchema (JT.Schema (HM.singleton "type" Null)) of
        [] -> HU.assertFailure "No checkSchema failures"
        _  -> pure ()
