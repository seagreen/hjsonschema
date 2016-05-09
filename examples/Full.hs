-- | Step by step validation using 'D4.referencesViaFilesystem' and
-- 'D4.checkSchema'. This means the actual validation invovles no IO.

module Full where

import           Data.Aeson

import qualified Data.JsonSchema.Draft4 as D4

schema :: D4.Schema
schema = D4.emptySchema { D4._schemaRef = Just "./unique.json" }

schemaContext :: D4.SchemaWithURI D4.Schema
schemaContext = D4.SchemaWithURI
  { D4._swSchema = schema
  , D4._swURI    = Just "./examples/json/imaginary.json"
  }

badData :: Value
badData = toJSON (["foo", "foo"] :: [String])

example :: IO ()
example = do
  res <- D4.referencesViaFilesystem schemaContext
  case res of
    Left _      -> error "Couldn't fetch referenced schemas."
    Right references -> do
      let validate = case D4.checkSchema references schemaContext of
                       Left _  -> error "Not a valid schema."
                       Right f -> f
      case validate badData of
        [] -> error "We validated bad data."
        [D4.Invalid (D4.Ref D4.UniqueItems) _ _] -> return () -- Success.
        e -> error "We got a different failure than expected."
