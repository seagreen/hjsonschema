
module Main where

import           Protolude

import           Control.Concurrent.Async       (withAsync)
import qualified Data.List.NonEmpty             as NE
import           Network.Wai.Application.Static (defaultFileServerSettings,
                                                 staticApp)
import           Network.Wai.Handler.Warp       (run)
import           Test.Hspec

import qualified JSONSchema.Draft4              as D4
import qualified JSONSchema.Types               as JT
import           Shared

-- Examples
import qualified AlternateSchema

dir :: FilePath
dir = "JSON-Schema-Test-Suite/tests/draft4"

main :: IO ()
main = withAsync serve $ \_ -> do
    ts <- readSchemaTests dir isHTTPTest
    hspec $ do
        describe "Language agnostic tests (using the record based schema)"
            (traverse_ (toTest validate) ts)
        describe "Language agnostic tests (using the 'Value' based schema)"
            (traverse_ (toTest validateExample) ts)
  where
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
            Left e          -> panic ("Remote.validateExample error: " <> show e)
            Right schemaMap -> do
                let failures = AlternateSchema.validate
                                   schemaMap
                                   (D4.SchemaWithURI s Nothing)
                                   (_scData sc)
                assertResult sc failures

serve :: IO ()
serve = run 1234
      . staticApp
      . defaultFileServerSettings
      $ "JSON-Schema-Test-Suite/remotes"
