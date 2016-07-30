{-# LANGUAGE TemplateHaskell #-}

module Data.JsonSchema.Draft4
    ( -- * Draft 4 Schema
      SchemaWithURI(..)
    , Schema(..)
    , SC.emptySchema

      -- * One-step validation (getting references over HTTP)
    , fetchHTTPAndValidate
    , HTTPValidationFailure(..)
    , FE.HTTPFailure(..)
    , InvalidSchema

      -- * One-step validation (getting references from the filesystem)
    , fetchFilesystemAndValidate
    , FilesystemValidationFailure(..)
    , FE.FilesystemFailure(..)

      -- * Validation failure
    , Invalid
    , Failure
    , FR.Fail(..)
    , ValidatorChain(..)

      -- * Fetching tools
    , ReferencedSchemas(..)
    , referencesViaHTTP
    , referencesViaFilesystem

      -- * Other Draft 4 things exported just in case
    , metaSchema
    , metaSchemaBytes
    , schemaValidity
    , referencesValidity
    , checkSchema
    , draft4FetchInfo
    ) where

import           Import
import           Prelude

import           Control.Arrow                   (first, left)
import qualified Data.ByteString                 as BS
import           Data.FileEmbed                  (embedFile,
                                                  makeRelativeToProject)
import qualified Data.HashMap.Strict             as HM
import qualified Data.List.NonEmpty              as NE
import           Data.Maybe                      (fromMaybe)

import           Data.JsonSchema.Draft4.Failure  (Failure, Invalid,
                                                  InvalidSchema,
                                                  ValidatorChain(..))
import           Data.JsonSchema.Draft4.Schema   (Schema)
import qualified Data.JsonSchema.Draft4.Schema   as SC
import qualified Data.JsonSchema.Draft4.Spec     as Spec
import           Data.JsonSchema.Fetch           (ReferencedSchemas(..),
                                                  SchemaWithURI(..))
import qualified Data.JsonSchema.Fetch           as FE
import qualified Data.Validator.Failure          as FR

data HTTPValidationFailure
    = HVRequest FE.HTTPFailure
    | HVSchema  InvalidSchema
    | HVData    Invalid
    deriving Show

-- | Fetch recursively referenced schemas over HTTP, check that both the
-- original and referenced schemas are valid, and then validate data.
fetchHTTPAndValidate
    :: SchemaWithURI Schema
    -> Value
    -> IO (Either HTTPValidationFailure ())
fetchHTTPAndValidate sw v = do
    res <- referencesViaHTTP sw
    pure (g =<< f =<< left HVRequest res)
  where
    f :: FE.URISchemaMap Schema
      -> Either HTTPValidationFailure (Value -> [Failure])
    f references = left HVSchema (checkSchema references sw)

    g :: (Value -> [Failure]) -> Either HTTPValidationFailure ()
    g validate = case NE.nonEmpty (validate v) of
                     Nothing       -> Right ()
                     Just failures -> Left (HVData failures)

data FilesystemValidationFailure
    = FVRead   FE.FilesystemFailure
    | FVSchema InvalidSchema
    | FVData   Invalid
    deriving (Show, Eq)

-- | Fetch recursively referenced schemas from the filesystem, check that
-- both the original and referenced schemas are valid, and then
-- validate data.
fetchFilesystemAndValidate
    :: SchemaWithURI Schema
    -> Value
    -> IO (Either FilesystemValidationFailure ())
fetchFilesystemAndValidate sw v = do
    res <- referencesViaFilesystem sw
    pure (g =<< f =<< left FVRead res)
  where
    f :: FE.URISchemaMap Schema
      -> Either FilesystemValidationFailure (Value -> [Failure])
    f references = left FVSchema (checkSchema references sw)

    g :: (Value -> [Failure]) -> Either FilesystemValidationFailure ()
    g validate = case NE.nonEmpty (validate v) of
                     Nothing      -> Right ()
                     Just invalid -> Left (FVData invalid)

-- | An instance of 'Data.JsonSchema.Fetch.FetchInfo' specialized for
-- JSON Schema Draft 4.
draft4FetchInfo :: FE.FetchInfo Schema
draft4FetchInfo = FE.FetchInfo Spec.embedded SC._schemaId SC._schemaRef

-- | Fetch the schemas recursively referenced by a starting schema over HTTP.
referencesViaHTTP
    :: SchemaWithURI Schema
    -> IO (Either FE.HTTPFailure (FE.URISchemaMap Schema))
referencesViaHTTP = FE.referencesViaHTTP' draft4FetchInfo

-- | Fetch the schemas recursively referenced by a starting schema from
-- the filesystem.
referencesViaFilesystem
    :: SchemaWithURI Schema
    -> IO (Either FE.FilesystemFailure (FE.URISchemaMap Schema))
referencesViaFilesystem = FE.referencesViaFilesystem' draft4FetchInfo

-- | A helper function.
--
-- Checks if a schema and a set of referenced schemas are valid.
--
-- Return a function to validate data.
checkSchema
    :: FE.URISchemaMap Schema
    -> SchemaWithURI Schema
    -> Either InvalidSchema (Value -> [Failure])
checkSchema sm sw =
    case NE.nonEmpty failures of
        Nothing -> Right (Spec.validate (ReferencedSchemas (_swSchema sw) sm) sw)
        Just fs -> Left fs
  where
    failures :: [(Maybe Text, Failure)]
    failures = ((\v -> (Nothing, v)) <$> schemaValidity (_swSchema sw))
            <> (first Just <$> referencesValidity sm)

metaSchema :: Schema
metaSchema =
      fromMaybe (error "Schema decode failed (this should never happen)")
    . decodeStrict
    $ metaSchemaBytes

metaSchemaBytes :: BS.ByteString
metaSchemaBytes =
    $(makeRelativeToProject "src/draft4.json" >>= embedFile)

-- | Check that a schema itself is valid
-- (if so the returned list will be empty).
schemaValidity :: Schema -> [Failure]
schemaValidity =
    Spec.validate referenced (SchemaWithURI metaSchema Nothing) . toJSON
  where
    referenced :: ReferencedSchemas Schema
    referenced = ReferencedSchemas
                     metaSchema
                     (HM.singleton "http://json-schema.org/draft-04/schema"
                         metaSchema)

-- | Check that a set of referenced schemas are valid
-- (if so the returned list will be empty).
referencesValidity
  :: FE.URISchemaMap Schema
  -> [(Text, Failure)]
  -- ^ The first value in the tuple is the URI of a referenced schema.
referencesValidity = HM.foldlWithKey' f mempty
  where
    f :: [(Text, Failure)]
      -> Text
      -> Schema
      -> [(Text, Failure)]
    f acc k v = ((\a -> (k,a)) <$> schemaValidity v) <> acc
