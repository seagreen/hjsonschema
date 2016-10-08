
module Main where

import           Control.Applicative
import           Control.Monad          (unless)
import           Data.Aeson
import           Data.Foldable          (traverse_)
import qualified Data.List.NonEmpty     as N
import           Data.Monoid
import qualified System.Timeout         as TO
import           Test.Hspec
import           Test.QuickCheck        (property)

import qualified Data.JsonSchema.Draft4 as D4
import           Data.JsonSchema.Fetch  (ReferencedSchemas(..))
import qualified Data.JsonSchema.Types  as JT
import qualified Local.Failure
import qualified Local.Validation
import qualified Local.Reference
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

    hspec $ do
        describe "Examples" exampleTests
        describe "QuickCheck" quickCheckTests
        describe "Failure" Local.Failure.spec
        describe "Data.Validator.Reference" Local.Reference.spec
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
                           Left (D4.HVData a) -> N.toList a
                           other              -> error ("Local.validate error: "
                                                       <> show other)
        traverse_ (checkPointer (_scData sc)) failures
        assertResult sc failures

    validateExample :: JT.Schema -> SchemaTestCase -> Expectation
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

quickCheckTests :: Spec
quickCheckTests =
    it "schemas invert through JSON without change" $ do
        property invertSchema
  where
    invertSchema :: D4.Schema -> Bool
    invertSchema a = Just a == decode (encode a)

exampleTests :: Spec
exampleTests = do
    it "Full.example compiles successfully" Full.example
    it "Simple.example compiles successfully" Simple.example
