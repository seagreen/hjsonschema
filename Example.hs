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

-- Data.JsonSchema isn't designed to be imported qualified,
-- but we're doing it here to make it obvious what's coming
-- from the lib.

schemaData :: JS.RawSchema
schemaData = JS.RawSchema
  { JS._rsURI    = ""
  , JS._rsObject = schemaJSON
  }
  where
    schemaJSON :: HashMap Text Value
    schemaJSON = H.singleton "uniqueItems" (Bool True)

badData :: Value
badData = Array $ V.fromList ["foo", "foo"]

main :: IO ()
main = do

  let currentSchemaCache = H.empty
  newSchemaCache <- (<> currentSchemaCache) <$> fetchGraph schemaData

  schema <- compileSchema newSchemaCache schemaData
  checkResults (JS.validate schema badData)

  where

    -- Not necessary in this case, but it serves as an example.
    --
    -- Note that Graphs are used to handle internal as well as external references.
    -- So even if a schema doesn't reference outside documents, you still need
    -- to generate a real Graph (and not just use mempty) if it refences itself.
    fetchGraph :: JS.RawSchema -> IO JS.Graph
    fetchGraph rs = do
      eitherGraph <- JS.fetchReferencedSchemas JS.draft4 rs H.empty
      case eitherGraph of
        Left e      -> error $ "Failed to fetch graph with error: " <> T.unpack e
        Right graph -> return graph

    compileSchema :: JS.Graph -> JS.RawSchema -> IO (JS.Schema JS.Draft4Failure)
    compileSchema graph rs =
      case JS.compileDraft4 graph rs of
        Left failure -> error $ "Not a valid schema: " <> show failure
        Right schema -> return schema

    checkResults :: [JS.ValidationFailure JS.Draft4Failure] -> IO ()
    checkResults [] = error "OHNO we validated bad data!"
    checkResults [JS.ValidationFailure JS.UniqueItems (JS.FailureInfo x y)] =
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
