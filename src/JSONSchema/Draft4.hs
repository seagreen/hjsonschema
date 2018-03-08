{-# LANGUAGE TemplateHaskell #-}

module JSONSchema.Draft4
    ( -- * Draft 4 Schema
      SchemaWithURI(..)
    , Schema(..)
    , SC.emptySchema

      -- * One-step validation (getting references over HTTP)
    , fetchHTTPAndValidate
    , HTTPValidationFailure(..)
    , FE.HTTPFailure(..)
    , SchemaInvalid(..)

      -- * One-step validation (getting references from the filesystem)
    , fetchFilesystemAndValidate
    , FilesystemValidationFailure(..)
    , FE.FilesystemFailure(..)

      -- * Validation failure
    , Invalid(..)
    , ValidatorFailure(..)

      -- * Fetching tools
    , URISchemaMap(..)
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

import qualified Data.ByteString as BS
import           Data.FileEmbed (embedFile, makeRelativeToProject)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)

import           JSONSchema.Draft4.Failure (Invalid(..), SchemaInvalid(..),
                                            ValidatorFailure(..))
import           JSONSchema.Draft4.Schema (Schema)
import qualified JSONSchema.Draft4.Schema as SC
import qualified JSONSchema.Draft4.Spec as Spec
import           JSONSchema.Fetch (SchemaWithURI(..), URISchemaMap(..))
import qualified JSONSchema.Fetch as FE

data HTTPValidationFailure
    = HVRequest FE.HTTPFailure
    | HVSchema  SchemaInvalid
    | HVData    Invalid
    deriving Show

-- | Fetch recursively referenced schemas over HTTP, check that both the
-- original and referenced schemas are valid, then validate then data.
fetchHTTPAndValidate
    :: SchemaWithURI Schema
    -> Value
    -> IO (Either HTTPValidationFailure ())
fetchHTTPAndValidate sw v = do
    res <- referencesViaHTTP sw
    pure (g =<< f =<< first HVRequest res)
  where
    f :: FE.URISchemaMap Schema
      -> Either HTTPValidationFailure (Value -> [ValidatorFailure])
    f references = first HVSchema (checkSchema references sw)

    g :: (Value -> [ValidatorFailure]) -> Either HTTPValidationFailure ()
    g val = case NE.nonEmpty (val v) of
                Nothing       -> Right ()
                Just failures -> Left (HVData Invalid
                                     { _invalidSchema   = _swSchema sw
                                     , _invalidInstance = v
                                     , _invalidFailures = failures
                                     })

data FilesystemValidationFailure
    = FVRead   FE.FilesystemFailure
    | FVSchema SchemaInvalid
    | FVData   Invalid
    deriving (Show, Eq)

-- | Fetch recursively referenced schemas from the filesystem, check
-- that both the original and referenced schemas are valid, then validate
-- the data.
fetchFilesystemAndValidate
    :: SchemaWithURI Schema
    -> Value
    -> IO (Either FilesystemValidationFailure ())
fetchFilesystemAndValidate sw v = do
    res <- referencesViaFilesystem sw
    pure (g =<< f =<< first FVRead res)
  where
    f :: FE.URISchemaMap Schema
      -> Either FilesystemValidationFailure (Value -> [ValidatorFailure])
    f references = first FVSchema (checkSchema references sw)

    g :: (Value -> [ValidatorFailure]) -> Either FilesystemValidationFailure ()
    g val = case NE.nonEmpty (val v) of
                Nothing      -> Right ()
                Just invalid -> Left (FVData Invalid
                                    { _invalidSchema   = _swSchema sw
                                    , _invalidInstance = v
                                    , _invalidFailures = invalid
                                    })

-- | An instance of 'JSONSchema.Fetch.FetchInfo' specialized for
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

-- | Checks if a schema and a set of referenced schemas are valid.
--
-- Return a function to validate data.
checkSchema
    :: FE.URISchemaMap Schema
    -> SchemaWithURI Schema
    -> Either SchemaInvalid (Value -> [ValidatorFailure])
checkSchema sm sw =
    case NE.nonEmpty failures of
        Just fs -> Left (SchemaInvalid fs)
        Nothing -> Right (Spec.specValidate sm sw)
  where
    failures :: [(Maybe Text, NonEmpty ValidatorFailure)]
    failures =
        let refFailures = first Just <$> referencesValidity sm
        in case NE.nonEmpty (schemaValidity (_swSchema sw)) of
                     Nothing   -> refFailures
                     Just errs -> (Nothing,errs) : refFailures

metaSchema :: Schema
metaSchema =
      fromMaybe (panic "Schema decode failed (this should never happen)")
    . decodeStrict
    $ metaSchemaBytes

metaSchemaBytes :: BS.ByteString
metaSchemaBytes =
    $(makeRelativeToProject "src/draft4.json" >>= embedFile)

-- | Check that a schema itself is valid
-- (if so the returned list will be empty).
schemaValidity :: Schema -> [ValidatorFailure]
schemaValidity =
    Spec.specValidate schemaMap (SchemaWithURI metaSchema Nothing) . toJSON
  where
    schemaMap :: URISchemaMap Schema
    schemaMap =
        URISchemaMap (HM.singleton "http://json-schema.org/draft-04/schema"
                                   metaSchema)

-- | Check that a set of referenced schemas are valid
-- (if so the returned list will be empty).
referencesValidity
  :: FE.URISchemaMap Schema
  -> [(Text, NonEmpty ValidatorFailure)]
  -- ^ The first item of the tuple is the URI of a schema, the second
  -- is that schema's validation errors.
referencesValidity = HM.foldlWithKey' f mempty . FE._unURISchemaMap
  where
    f :: [(Text, NonEmpty ValidatorFailure)]
      -> Text
      -> Schema
      -> [(Text, NonEmpty ValidatorFailure)]
    f acc k v = case NE.nonEmpty (schemaValidity v) of
                    Nothing   -> acc
                    Just errs -> (k,errs) : acc
