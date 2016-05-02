{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.JsonSchema.Draft4
  ( Schema(..)
  , emptySchema

    -- * One-step validation
  , HTTPValidationFailure(..)
  , fetchHTTPAndValidate
  , FilesystemValidationFailure(..)
  , fetchFilesystemAndValidate

    -- * Fetching tools
  , SchemaWithURI(..)
  , ReferencedSchemas(..)
  , HTTPFailure(..)
  , referencesViaHTTP
  , FilesystemFailure(..)
  , referencesViaFilesystem

    -- * Failure
  , Failure(..)
  , ValidatorChain(..)

    -- * Other Draft 4 things exported just in case
  , checkSchema
  , schemaValidity
  , IN.runValidate
  , draft4Spec
  ) where

import           Control.Arrow                   (left)
import           Data.Aeson
import qualified Data.ByteString.Lazy            as LBS
import           Data.FileEmbed
import qualified Data.HashMap.Strict             as H
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.List.NonEmpty              as N
import           Data.Maybe                      (fromMaybe)

import           Data.JsonSchema.Draft4.Failure
import qualified Data.JsonSchema.Draft4.Internal as IN
import           Data.JsonSchema.Draft4.Schema
import           Data.JsonSchema.Fetch           (FilesystemFailure(..),
                                                  HTTPFailure(..),
                                                  ReferencedSchemas(..),
                                                  SchemaWithURI(..))
import qualified Data.JsonSchema.Fetch           as FE

data HTTPValidationFailure
  = HVRequest HTTPFailure
  | HVSchema  (NonEmpty Failure)
  | HVData    (NonEmpty Failure)
  deriving Show

fetchHTTPAndValidate
  :: SchemaWithURI Schema
  -> Value
  -> IO (Either HTTPValidationFailure ())
fetchHTTPAndValidate sw v = do
  res <- referencesViaHTTP sw
  pure (g =<< f =<< left HVRequest res)
  where
    f :: ReferencedSchemas Schema
      -> Either HTTPValidationFailure (Value -> [Failure])
    f references = left HVSchema (checkSchema references sw)

    g :: (Value -> [Failure]) -> Either HTTPValidationFailure ()
    g validate = case N.nonEmpty (validate v) of
                   Nothing       -> Right ()
                   Just failures -> Left (HVData failures)

data FilesystemValidationFailure
  = FVRead   FilesystemFailure
  | FVSchema (NonEmpty Failure)
  | FVData   (NonEmpty Failure)
  deriving Show

fetchFilesystemAndValidate
  :: SchemaWithURI Schema
  -> Value
  -> IO (Either FilesystemValidationFailure ())
fetchFilesystemAndValidate sw v = do
  res <- referencesViaFilesystem sw
  pure (g =<< f =<< left FVRead res)
  where
    f :: ReferencedSchemas Schema
      -> Either FilesystemValidationFailure (Value -> [Failure])
    f references = left FVSchema (checkSchema references sw)

    g :: (Value -> [Failure]) -> Either FilesystemValidationFailure ()
    g validate = case N.nonEmpty (validate v) of
                   Nothing       -> Right ()
                   Just failures -> Left (FVData failures)

-- | Check the validity of a schema and return a function to validate data.
checkSchema
  :: ReferencedSchemas Schema
  -> SchemaWithURI Schema
  -> Either (NonEmpty Failure) (Value -> [Failure])
checkSchema referenced schemaWithURI =
  case N.nonEmpty . schemaValidity . _swSchema $ schemaWithURI of
    Nothing       -> Right (IN.runValidate referenced schemaWithURI)
    Just failures -> Left failures

draft4Spec :: FE.Spec Schema
draft4Spec = FE.Spec IN.embedded _schemaId _schemaRef

referencesViaHTTP
  :: SchemaWithURI Schema
  -> IO (Either HTTPFailure (ReferencedSchemas Schema))
referencesViaHTTP = FE.referencesViaHTTP' draft4Spec

referencesViaFilesystem
  :: SchemaWithURI Schema
  -> IO (Either FilesystemFailure (ReferencedSchemas Schema))
referencesViaFilesystem = FE.referencesViaFilesystem' draft4Spec

-- | In normal situations just use 'checkSchema', which is a combination of
-- 'schemaValidity' and 'runValidate'.
schemaValidity :: Schema -> [Failure]
schemaValidity = IN.runValidate referenced (SchemaWithURI d4 Nothing) . toJSON
  where
    d4 :: Schema
    d4 = fromMaybe (error "Schema decode failed (this should never happen)")
       . decode . LBS.fromStrict $ $(embedFile "src/draft4.json")

    referenced :: ReferencedSchemas Schema
    referenced = ReferencedSchemas
                   d4
                   (H.singleton "http://json-schema.org/draft-04/schema" d4)
