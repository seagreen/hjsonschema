{-# LANGUAGE ScopedTypeVariables #-}

module Data.JsonSchema.Core where

import qualified Data.HashMap.Strict       as H
import           Data.Maybe

import           Data.JsonSchema.Reference
import           Import

--------------------------------------------------
-- * Primary API
--------------------------------------------------

compile :: forall err. Spec err -> Graph -> RawSchema -> Schema err
compile spec g (RawSchema t o) =
  let maybeValidators = H.intersectionWith f (_unSpec spec) o
  in Schema . catMaybes . H.elems $ maybeValidators
  where
    f :: ValSpec err -> Value -> Maybe (Value -> [ValidationFailure err])
    f (ValSpec _ construct) valJSON = construct spec g (RawSchema (newResolutionScope t o) o) valJSON

validate :: Schema err -> Value -> [ValidationFailure err]
validate schema x = concat . fmap ($ x) . _unSchema $ schema

--------------------------------------------------
-- * Schemas
--------------------------------------------------

newtype Spec err = Spec { _unSpec :: HashMap Text (ValSpec err) }

newtype Schema err = Schema { _unSchema :: [Value -> [ValidationFailure err]] }

data RawSchema = RawSchema
  { _rsURI    :: Text
  , _rsObject :: HashMap Text Value
  }

-- | A mapping of URLs to schemas.
--
-- Each key/value pair provides the components of a RawSchema.
type Graph = HashMap Text (HashMap Text Value)

--------------------------------------------------
-- * Validators
--------------------------------------------------

data ValSpec err = ValSpec EmbeddedSchemas (ValidatorConstructor err [ValidationFailure err])

-- | Return a schema's immediate subschemas.
--
-- This is used by 'Data.JsonSchema.fetchRefs' to find all the
-- subschemas in a document. This allows it to process only
-- "$ref"s and "id"s that are actual schema keywords.
type EmbeddedSchemas = Text -> Value -> Vector RawSchema

-- | This is what's used to write most validators in practice.
--
-- Its important that particular validators don't know about the error sum type
-- of the Spec they're going to be used in. That way they can be included in
-- other Specs later without encouraging partial functions.
--
-- This means that a properly written ValidatorConstructor will need its error
-- type modified for use in a Spec. Data.JsonSchema.Helpers provides giveName
-- and modifyName for this purpose.
type ValidatorConstructor schemaErr valErr
   = Spec schemaErr
  -> Graph
  -> RawSchema
  -> Value
  -> Maybe (Value -> valErr)

data ValidationFailure err = ValidationFailure
  { _failureName :: err
  , _failureInfo :: FailureInfo
  } deriving (Show, Read)

data FailureInfo = FailureInfo
  { _validatingData :: Value
  , _offendingData  :: Value
  } deriving (Show, Read)
