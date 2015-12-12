{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.JsonSchema.Draft4
  ( Schema(..)
  , emptySchema
  , checkSchema

    -- * Fetching tools
  , SchemaContext(..)
  , SchemaCache(..)
  , fetchReferencedSchemas

    -- * Failure
  , Failure(..)
  , ValidatorChain(..)

    -- * Other Draft 4 things exported just in case
  , schemaValidity
  , IN.runValidate
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy            as LBS
import           Data.FileEmbed
import qualified Data.HashMap.Strict             as H
import           Data.Maybe                      (fromMaybe)

import           Data.JsonSchema.Draft4.Failure
import qualified Data.JsonSchema.Draft4.Internal as IN
import           Data.JsonSchema.Draft4.Schema
import           Data.JsonSchema.Fetch           (SchemaCache(..),
                                                  SchemaContext(..),
                                                  URISchemaMap)
import qualified Data.JsonSchema.Fetch           as FE
import           Data.Validator.Reference        (baseAndFragment)
import           Import

-- | Check the validity of a schema and return a function to validate data.
checkSchema
  :: SchemaCache Schema
  -> SchemaContext Schema
  -> Either [Failure] (Value -> [Failure])
checkSchema sg sc =
  case schemaValidity (_scSchema sc) of
    [] -> Right (IN.runValidate sg sc)
    es -> Left es

fetchReferencedSchemas
  :: URISchemaMap Schema
  -> SchemaContext Schema
  -> IO (Either Text (SchemaCache Schema))
fetchReferencedSchemas =
  FE.fetchReferencedSchemas IN.embedded _schemaId _schemaRef

-- | In normal situations just use 'checkSchema', which is a combination of
-- 'schemaValidity' and 'runValidate'.
schemaValidity :: Schema -> [Failure]
schemaValidity = IN.runValidate cache (SchemaContext Nothing d4) . toJSON
  where
    d4 :: Schema
    d4 = fromMaybe (error "Schema decode failed (this should never happen)")
       . decode . LBS.fromStrict $ $(embedFile "draft4.json")

    -- @fst . baseAndFragment@ is necessary to remove the trailing "#",
    -- otherwise cache lookups to resolve internal references will fail.
    cache :: SchemaCache Schema
    cache = SchemaCache d4 $ case _schemaId d4 >>= fst . baseAndFragment of
                               Nothing  -> mempty
                               Just uri -> H.singleton uri d4
