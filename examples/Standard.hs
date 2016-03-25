{-# LANGUAGE OverloadedStrings #-}

module Standard where

import           Data.Aeson
import qualified Data.HashMap.Strict    as H
import qualified Data.Vector            as V

import qualified Data.JsonSchema.Draft4 as D4

schema :: D4.Schema
schema = D4.emptySchema { D4._schemaUniqueItems = Just True }

schemaContext :: D4.SchemaContext D4.Schema
schemaContext = D4.SchemaContext
  { D4._scURI    = Nothing
  -- ^ If your schema has relative references to other schemas
  -- then you'll need to give its URI here.
  , D4._scSchema = schema
  }

badData :: Value
badData = Array (V.fromList ["foo", "foo"])

example :: IO ()
example = do
  -- Since we know our schema doesn't reference any other schemas we could
  -- skip this step and use @SchemaCache schema mempty@ instead.
  --
  -- If we make a mistake and out schema does include references to other
  -- schemas then those references will always return 'D4.RefResolution'
  -- validation failures.
  cache <- makeCache

  let validate = case D4.checkSchema cache schemaContext of
                   Left _  -> error "Not a valid schema."
                   Right f -> f

  case validate badData of
    [] -> error "We validated bad data."
    [D4.Failure D4.UniqueItems _ _] -> return () -- Success.
    _ -> error "We got a different failure than expected."

  where
    makeCache :: IO (D4.SchemaCache D4.Schema)
    makeCache = do
      -- A HashMap of URIs (in the form of Text) to schemas.
      let currentlyStoredSchemas = H.empty
      res <- D4.fetchReferencedSchemas currentlyStoredSchemas schemaContext
      case res of
        Left _      -> error "Couldn't fetch referenced schemas."
        Right cache -> return cache
