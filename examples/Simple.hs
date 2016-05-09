-- | Fetch any referenced schemas, check that our original schema is itself
-- valid, then validate our data.
--
-- To fetch schemas using HTTP instead of from the filesystem use
-- 'D4.fetchHTTPAndValidate'.

module Simple where

import           Data.Aeson
import qualified Data.List.NonEmpty as NE

import qualified Data.JsonSchema.Draft4 as D4

schema :: D4.Schema
schema = D4.emptySchema { D4._schemaRef = Just "./unique.json" }

schemaContext :: D4.SchemaWithURI D4.Schema
schemaContext = D4.SchemaWithURI
  { D4._swSchema = schema
  , D4._swURI    = Just "./examples/json/imaginary.json"
  -- ^ For this example we're pretending we found 'schema' at this location.
  -- Its relative links will be resolved from here.
  }

badData :: Value
badData = toJSON (["foo", "foo"] :: [String])

example :: IO ()
example = do
  res <- D4.fetchFilesystemAndValidate schemaContext badData
  case res of
    Right () -> error "We validated bad data."
    Left (D4.FVRead _) -> error ("Error fetching a referenced schema"
                                ++ " (either during IO or parsing).")
    Left (D4.FVSchema _) -> error "Our 'schema' itself was invalid."
    Left (D4.FVData failures) ->
      case NE.toList failures of
        [D4.Invalid (D4.Ref D4.UniqueItems) _ _] -> return () -- Success.
        _ -> error "Got more invalidation reasons than we expected."
