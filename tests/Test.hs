{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy           as LBS
import           Data.Char                      (toLower)
import qualified Data.HashMap.Strict            as H
import           Data.JsonSchema
import           Data.List                      (isSuffixOf)
import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import           System.Directory               (getDirectoryContents)
import           System.FilePath                ((</>))
import           Test.Framework
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit                     as HU

main :: IO ()
main = do
  ts <- readSchemaTests
  defaultMain (toTest <$> ts)

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
    return $ RawSchema "" o

instance FromJSON SchemaTest where
  parseJSON = withObject "SchemaTest" $ \o -> SchemaTest
    <$> o .: "description"
    <*> o .: "schema"
    <*> o .: "tests" -- I wish this were "cases"

readSchemaTests :: IO [SchemaTest]
readSchemaTests = do
  jsonFiles <- filter (".json" `isSuffixOf`) <$> getDirectoryContents dir
  fmap concat $ forM jsonFiles $ \file -> do
    let fullPath = dir </> file
    jsonBS <- LBS.readFile fullPath
    case eitherDecode jsonBS of
      Left err      -> fail $ "couldn't parse file '" <> fullPath <> "': " <> err
      Right schemas -> return $ prependFileName file <$> schemas
  where
    dir = "JSON-Schema-Test-Suite/tests/draft4"
    prependFileName :: String -> SchemaTest -> SchemaTest
    prependFileName fileName s = s
      { _stDescription = T.pack fileName <> ": " <> _stDescription s
      }

toTest :: SchemaTest -> Test
toTest st = testGroup groupName (mkCase <$> _stCases st)
  where
    groupName :: String
    groupName = T.unpack $ _stDescription st

    mkCase :: SchemaTestCase -> Test
    mkCase sc = testCase caseName assertion
      where
        caseName = T.unpack $ _scDescription sc
        assertion = if _scValid sc
          then assertValid   (_stSchema st) (_scData sc)
          else assertInvalid (_stSchema st) (_scData sc)

assertValid, assertInvalid :: RawSchema -> Value -> HU.Assertion
assertValid r v = do
  g <- fetchRefs r H.empty
  let es = validate (compile draft4 g r) v
  unless (V.length es == 0) $ HU.assertFailure (show es)
assertInvalid r v = do
  g <- fetchRefs r H.empty
  let es = validate (compile draft4 g r) v
  when (V.length es == 0) $ HU.assertFailure "expected a validation error"

$(deriveFromJSON defaultOptions { fieldLabelModifier = map toLower . drop 3 } ''SchemaTestCase)
