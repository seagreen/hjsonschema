
module Main where

import           Protolude

import           Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified System.Timeout     as TO
import           Test.Hspec
import           Test.QuickCheck    (property)

import qualified JSONSchema.Draft4  as D4
import qualified JSONSchema.Types   as JT
import qualified Local.Failure
import qualified Local.Validation
import qualified Local.Reference
import           Shared

-- Examples
import qualified AlternateSchema
import qualified Simple
import qualified TwoStep

dir :: FilePath
dir = "JSON-Schema-Test-Suite/tests/draft4"

supplementDir :: FilePath
supplementDir = "test/supplement"

main :: IO ()
main = do

    -- Language agnostic tests
    ts <- readSchemaTests
              dir
              (\a -> not (isHTTPTest a || skipTest a))

    -- Custom supplements to the language agnostic tests
    supplementTs <- readSchemaTests supplementDir (not . isHTTPTest)

    hspec $ do
        describe "Examples" exampleTests
        describe "QuickCheck" quickCheckTests
        describe "Failure" Local.Failure.spec
        describe "JSONSchema.Validator.Reference" Local.Reference.spec
        describe "Supplementary validation tests written in Haskell"
            Local.Validation.spec

        describe "Supplementary tests written in JSON (using the record based schema)"
            (traverse_ (toTest (fmap timeout . validate)) supplementTs)
        describe "Supplementary tests written in JSON (using the 'Value' based schema)"
            (traverse_ (toTest (fmap timeout . validateExample)) supplementTs)

        describe "Language agnostic tests (using the record based schema)"
            (traverse_ (toTest validate) ts)
        describe "Language agnostic tests (using the 'Value' based schema)"
            (traverse_ (toTest validateExample) ts)
  where
    timeout :: Expectation -> Expectation
    timeout f = do
        res <- TO.timeout 3000000 f
        case res of
            Nothing -> expectationFailure "timeout expired"
            Just a  -> pure a

    validate :: D4.Schema -> SchemaTestCase -> Expectation
    validate s sc = do
        res <- D4.fetchHTTPAndValidate (D4.SchemaWithURI s Nothing) (_scData sc)
        let failures = case res of
                           Right ()           -> mempty
                           Left (D4.HVData a) -> NE.toList (D4._invalidFailures a)
                           other              -> panic ("Local.validate error: "
                                                       <> show other)
        assertResult sc failures

    validateExample :: JT.Schema -> SchemaTestCase -> Expectation
    validateExample s sc = do
        res <- AlternateSchema.referencesViaHTTP (D4.SchemaWithURI s Nothing)
        case res of
            Left e          -> panic ("Local.validateExample error: " <> show e)
            Right schemaMap -> do
                let failures = AlternateSchema.validate
                                   schemaMap
                                   (D4.SchemaWithURI s Nothing)
                                   (_scData sc)
                assertResult sc failures

quickCheckTests :: Spec
quickCheckTests =
    it "schemas invert through JSON without change" $ do
        property invertSchema
  where
    invertSchema :: D4.Schema -> Bool
    invertSchema a = Just a == decode (encode a)

exampleTests :: Spec
exampleTests = do
    it "Simple.example compiles successfully" Simple.example
    it "TwoStep.example compiles successfully" TwoStep.example
