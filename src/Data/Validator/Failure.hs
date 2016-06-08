{-# LANGUAGE DeriveFunctor #-}

module Data.Validator.Failure where

import           Import
-- Hiding is for GHCs before 7.10:
import           Prelude            hiding (concat, sequence)

import qualified Data.Aeson.Pointer as AP

-- | Validators shouldn't know more about the schema they're going to
-- be used with than necessary. If a validator throws errors using the
-- error sum type of a particular schema, then it can't be used with
-- other schemas later that have different error sum types (at least not
-- without writing partial functions).
--
-- Because of this we make 'Fail' a higher order type, so each validator
-- can return a sum type describing only the failures that can occur in that
-- validator (or '()' if that validator can only fail in one way).
--
-- It's the job of a schema's validate function to unify the errors produced
-- by the validators it uses into a single error sum type for that schema.
-- The schema's validate function will return a 'Fail' with
-- that sum type as its type argument.
--
-- The slightly weird naming ('Fail' and 'Failure') is so that we can define
-- a 'type Failure = Fail SchemaErrorType' for each of our schemas, and
-- export it along with 'Fail(..)'. This way the users of the library only
-- use 'Failure', not 'Fail'.
data Fail err = Failure
    { _failureValidatorsCalled :: !err
      -- ^ E.g. @Items UniqueItems@ during draft 4 validation.
    , _failureFinalValidator   :: !Value
      -- ^ The value of the validator that raised the error (e.g. the value of
      -- @"uniqueItems"@ in the above example.
    , _failureOffendingPointer :: !AP.Pointer
      -- ^ A pointer to the part of the data that caused invalidation.
    , _failureOffendingData    :: !Value
      -- ^ The part of the data that caused invalidation. Usually this is
      -- identical to the result of resolving '_invalidOffendingPointer'
      -- against the starting data, but not always (e.g. in the case of
      -- 'additionalItems' where '_invalidOffendingData' will be the items
      -- in the array that were not allowed, instead of the entire array).
    } deriving (Eq, Show, Functor)

prependToPath :: AP.Token -> Fail a -> Fail a
prependToPath tok failure =
    let old = _failureOffendingPointer failure
    in failure { _failureOffendingPointer = AP.Pointer [tok] <> old }
