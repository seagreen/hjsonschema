
module Local.Filesystem where

import           Data.Aeson
import           Data.Monoid
import           Test.Tasty             (TestTree)
import qualified Test.Tasty.HUnit       as HU
import           Data.Text              (Text)

import           Data.JsonSchema.Draft4

fetchFromFilesystem :: [TestTree]
fetchFromFilesystem =
  [ HU.testCase
      "readFile exceptions are turned into Lefts"
      readFileExceptions

  , HU.testCase
      "Relative reference to local file"
      (resolve "tests/Local/schema.json")
  , HU.testCase
      "Chained relative references to local files"
      (resolve "./tests/Local/schema-with-ref.json")
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

-- * Helpers

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
