-- | Differences from @examples/Simple.hs@:
--
-- * We demonstrate how to do the validation step without IO by first
-- getting the references with 'D4.referencesViaFilesystem'.
--
-- * This shows how to write the starting schema in Haskell instead
-- of parsing it from JSON.
--
-- * This schema references other schemas. Previously we used
-- 'fetchHTTPAndValidate' which fetched references over HTTP (though it
-- didn't matter because the last schema didn't actually have any).
-- This time we need to get them from the filesystem.

module TwoStep where

import           Protolude

import           Data.Aeson                  (Value (..), toJSON)
import qualified Data.List.NonEmpty          as NE

import qualified JSONSchema.Draft4           as D4
import qualified JSONSchema.Validator.Draft4 as VAL

schema :: D4.Schema
schema = D4.emptySchema { D4._schemaRef = Just "./unique.json" }

schemaContext :: D4.SchemaWithURI D4.Schema
schemaContext = D4.SchemaWithURI
    { D4._swSchema = schema
    , D4._swURI    = Just "./examples/json/start.json"
    }

badData :: Value
badData = toJSON [True, True]

example :: IO ()
example = do
    res <- D4.referencesViaFilesystem schemaContext
    let references = case res of
                         Left _  -> panic "Couldn't fetch referenced schemas."
                         Right a -> a
        validate = case D4.checkSchema references schemaContext of
                       Left _  -> panic "Not a valid schema."
                       Right f -> f
    case validate badData of
        [] -> panic "We validated bad data."
        [D4.FailureRef (VAL.RefInvalid _ _ (D4.FailureUniqueItems _ NE.:|[])) ] ->
            pure () -- Success
        _ -> panic "We got a different failure than expected."
