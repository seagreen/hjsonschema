-- | Demonstrate 'D4.fetchFilesystemAndValidate'.
--
-- To fetch schemas using HTTP instead of from the filesystem use
-- 'D4.fetchHTTPAndValidate'.

module Simple where

import           Data.Aeson             (Value(..), decode, toJSON)
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe             (fromMaybe)

import qualified Data.JsonSchema.Draft4 as D4

badData :: Value
badData = toJSON (["foo", "foo"] :: [String])

example :: IO ()
example = do

    -- Get the starting schema.
    bts <- LBS.readFile "./examples/json/start.json"
    let schema = fromMaybe (error "Invalid schema JSON.") (decode bts)
        schemaWithURI = D4.SchemaWithURI schema (Just "./examples/json/start.json")

    -- Fetch any referenced schemas, check that our original schema itself
    -- is valid, and validate the data.
    res <- D4.fetchFilesystemAndValidate schemaWithURI badData
    case res of
        Right ()                  -> error "We validated bad data."
        Left (D4.FVRead _)        -> error ("Error fetching a referenced schema"
                                            ++ " (either during IO or parsing).")
        Left (D4.FVSchema _)      -> error "Our 'schema' itself was invalid."
        Left (D4.FVData failures) ->
            case NE.toList failures of
                [D4.Failure (D4.Ref D4.UniqueItems) _ _ _] -> return () -- Success.
                _ -> error "Got more invalidation reasons than we expected."
