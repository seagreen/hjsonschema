{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Aeson
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Data.JsonSchema as JS

-- Data.JsonSchema is designed to be imported unqualified.
-- We're just being explicit here for example purposes.

main :: IO ()
main = do
  simpleExample
  fullExample

--------------------------------------------------
-- * Example Schema and Data
--------------------------------------------------

schemaData :: JS.RawSchema
schemaData = JS.RawSchema
  { JS._rsURI  = Nothing
  -- ^ If your schema has relative references to other schemas
  -- then you'll need to include this.
  , JS._rsData = schemaJSON
  }
  where
    schemaJSON :: HashMap Text Value
    schemaJSON = H.singleton "uniqueItems" (Bool True)

badData :: Value
badData = Array $ V.fromList ["foo", "foo"]

--------------------------------------------------
-- * Simple Example
--------------------------------------------------

-- | Since our schema contains no references to other schemas,
-- we can skip the fetchGraph step.
simpleExample :: IO ()
simpleExample = do
  schema <- compileSchema (JS.SchemaGraph schemaData H.empty) schemaData
  checkResults (JS.validate schema badData)

--------------------------------------------------
-- * Full Example
--------------------------------------------------

-- | For a schema that might contain references to other schemas.
fullExample :: IO ()
fullExample = do
  let currentSchemaCache = H.empty
  schemaGraph <- fetchGraph currentSchemaCache schemaData

  -- We aren't going to use it for anything this time, but if we were caching
  -- remote schemas this would be our new cache.
  --
  -- let newSchemaCache = _cachedSchemas schemaData <> currentSchemaGraph

  schema <- compileSchema schemaGraph schemaData
  checkResults (JS.validate schema badData)

  where
    fetchGraph :: HashMap Text (HashMap Text Value) -> JS.RawSchema -> IO JS.SchemaGraph
    fetchGraph graph rs = do
      eitherGraph <- JS.fetchReferencedSchemas JS.draft4 graph rs
      case eitherGraph of
        Left e  -> error $ "Failed to fetch graph with error: " <> T.unpack e
        Right g -> return g

--------------------------------------------------
-- * Helper Functions
--------------------------------------------------

compileSchema :: JS.SchemaGraph -> JS.RawSchema -> IO (JS.Schema JS.Draft4Failure)
compileSchema graph rs =
  case JS.compileDraft4 graph rs of
    Left failure -> error $ "Not a valid schema: " <> show failure
    Right schema -> return schema

checkResults :: [JS.ValidationFailure JS.Draft4Failure] -> IO ()
checkResults [] = error "OHNO we validated bad data!"
checkResults [JS.ValidationFailure JS.UniqueItems (JS.FailureInfo x y [])] =
  putStrLn . unlines $
    [ ""
    , "Success. We got a UniqueItems error as expected."
    , "Here's the relevant part of the bad JSON document:"
    , ""
    , "  " <> show y
    , ""
    , "And here's the content of the validator that caught it:"
    , ""
    , "  " <> show x
    ]
checkResults x = error $ "OHNO we got a different failure than we expected: " <> show x
