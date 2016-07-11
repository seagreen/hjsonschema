-- | Two differences from @examples/Simple.hs@:
--
-- * This shows how to write the starting schema in Haskell instead
-- of parsing it from JSON.
--
-- * Validation is done in two steps using 'D4.referencesViaFilesystem' and
-- 'D4.checkSchema' instead of 'D4.fetchFilesystemAndValidate'. This means
-- that the actual validation involves no IO.

module Full where

import           Data.Aeson             (Value (..), toJSON)

import qualified Data.JsonSchema.Draft4 as D4

schema :: D4.Schema
schema = D4.emptySchema { D4._schemaRef = Just "./unique.json" }

schemaContext :: D4.SchemaWithURI D4.Schema
schemaContext = D4.SchemaWithURI
    { D4._swSchema = schema
    , D4._swURI    = Just "./examples/json/start.json"
    }

badData :: Value
badData = toJSON (["foo", "foo"] :: [String])

example :: IO ()
example = do
    res <- D4.referencesViaFilesystem schemaContext
    let references = case res of
                         Left _  -> error "Couldn't fetch referenced schemas."
                         Right a -> a
        validate = case D4.checkSchema references schemaContext of
                       Left _  -> error "Not a valid schema."
                       Right f -> f
    case validate badData of
        [] -> error "We validated bad data."
        [D4.Failure (D4.Ref D4.UniqueItems) _ _ _] -> return () -- Success
        _ -> error "We got a different failure than expected."
