
module Simple where

import           Protolude

import           Data.Aeson           (Value(..), decode, toJSON)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty   as NE
import           Data.Maybe           (fromMaybe)

import qualified JSONSchema.Draft4    as D4

badData :: Value
badData = toJSON [True, True]

example :: IO ()
example = do
    bts <- LBS.readFile "./examples/json/unique.json"
    let schema = fromMaybe (panic "Invalid schema JSON.") (decode bts)
        schemaWithURI = D4.SchemaWithURI
                            schema
                            Nothing -- This would be the URI of the schema
                                    -- if it had one. It's used if the schema
                                    -- has relative references to other
                                    -- schemas.

    -- Fetch any referenced schemas over HTTP, check that our original schema
    -- itself is valid, and validate the data.
    res <- D4.fetchHTTPAndValidate schemaWithURI badData
    case res of
        Right ()                  -> panic "We validated bad data."
        Left (D4.HVRequest _)     -> panic ("Error fetching a referenced schema"
                                            <> " (either during IO or parsing).")
        Left (D4.HVSchema _)      -> panic "Our 'schema' itself was invalid."
        Left (D4.HVData (D4.Invalid _ _ failures)) ->
            case NE.toList failures of
                [D4.FailureUniqueItems _] -> pure () -- Success.
                _ -> panic "We got a different failure than expected."
