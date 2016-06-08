
module Main where

import           Control.Applicative
import           Control.Monad          (unless)
import           Data.Aeson
import           Data.Foldable          (traverse_)
import qualified Data.List.NonEmpty     as N
import           Data.Monoid
import qualified System.Timeout         as TO
import           Test.Tasty             (TestTree, defaultMain, testGroup)
import qualified Test.Tasty.HUnit       as HU
import           Test.Tasty.QuickCheck  (testProperty)

import qualified Data.JsonSchema.Draft4 as D4
import           Data.JsonSchema.Fetch  (ReferencedSchemas(..))
import qualified Data.JsonSchema.Types  as JT
import           Local.Failure          (correctPaths)
import           Local.Validation       (fetchFromFilesystem,
                                         generalValidation)
import           Local.Reference        (referenceTests)
import           Shared

-- Examples
import qualified AlternateSchema
import qualified Full
import qualified Simple

dir :: String
dir = "JSON-Schema-Test-Suite/tests/draft4"

supplementDir :: String
supplementDir = "test/supplement"

main :: IO ()
main = do

    -- Language agnostic tests
    ts <- readSchemaTests
              dir
              (\a -> not (isHTTPTest a || skipOptional a))

    -- Custom supplements to the language agnostic tests
    supplementTs <- readSchemaTests
                        supplementDir
                        (\a -> not (isHTTPTest a || skipOptional a))

    defaultMain . testGroup "Tests not requiring an HTTP server" $
        [ testGroup
            "Check that examples compile and don't throw errors"
            exampleTests
        , testGroup
            "QuickCheck tests"
            quickCheckTests
        , testGroup
            "Report the path to invalid data correctly"
            correctPaths
        , testGroup
            "Test the Reference module"
            referenceTests
        , testGroup
            "Test the referencesViaFilesystem function"
            fetchFromFilesystem
        , testGroup
            "Supplementary validation tests written in Haskell"
            generalValidation
        , testGroup
            "Supplementary tests written in JSON (using the record based schema)"
            (toTest (fmap timeout . validate) <$> supplementTs)
        , testGroup
            "Supplementary tests written in JSON (using the 'Value' based schema)"
            (toTest (fmap timeout . validateExample) <$> supplementTs)
        , testGroup
            "Language agnostic local tests (using the record based schema)"
            (toTest validate <$> ts)
        , testGroup
            "Language agnostic local tests (using the 'Value' based schema)"
            (toTest validateExample <$> ts)
        ]
  where
    timeout :: HU.Assertion -> HU.Assertion
    timeout f = do
        res <- TO.timeout 3000000 f
        case res of
            Nothing -> HU.assertFailure "timeout expired"
            Just a  -> pure a

    validate :: D4.Schema -> SchemaTestCase -> HU.Assertion
    validate s sc = do
        res <- D4.fetchHTTPAndValidate (D4.SchemaWithURI s Nothing) (_scData sc)
        let failures = case res of
                           Right ()           -> mempty
                           Left (D4.HVData a) -> N.toList a
                           other              -> error ("Local.validate error: "
                                                       <> show other)
        traverse_ (checkPointer (_scData sc)) failures
        assertResult sc failures

    validateExample :: JT.Schema -> SchemaTestCase -> HU.Assertion
    validateExample s sc = do
        res <- AlternateSchema.referencesViaHTTP (D4.SchemaWithURI s Nothing)
        case res of
            Left e          -> error ("Local.validateExample error: " <> show e)
            Right schemaMap -> do
                let failures = AlternateSchema.validate
                                   (ReferencedSchemas s schemaMap)
                                   Nothing s (_scData sc)
                traverse_ (checkPointer (_scData sc)) failures
                assertResult sc failures

quickCheckTests :: [TestTree]
quickCheckTests =
    [testProperty "Invert schemas through JSON without change" invertSchema]
  where
    invertSchema :: D4.Schema -> Bool
    invertSchema a = Just a == decode (encode a)

exampleTests :: [TestTree]
exampleTests =
    [ HU.testCase "Full example" Full.example
    , HU.testCase "Simple example" Simple.example
    ]
