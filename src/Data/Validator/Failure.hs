{-# LANGUAGE ScopedTypeVariables #-}

module Data.Validator.Failure where

import qualified Data.Aeson.Pointer as P

import           Import

-- For GHCs before 7.10:
import           Prelude            hiding (concat, sequence)

-- | Validators shouldn't know more about the schema they're going to
-- be used with than necessary. If a validator throws errors using the
-- error sum type of a particular schema, then it can't be used with
-- other schemas later that have different error sum types (at least not
-- without writing partial functions).
--
-- Thus validators that can only fail in one way return 'FailureInfo's.
-- Validators that can fail in multiple ways return 'ValidationFailure's
-- along with an custom error sum type for that particular validator.
--
-- It's the job of a schema's validate function to unify the errors produced
-- by the validators it uses into a single error sum type for that schema.
-- The schema's validate function will return a 'ValidationFailure' with
-- that sum type as its type argument.
data Failure err = Invalid
  { _invalidValidatorsCalled :: !err
  , _invalidFinalValidator   :: !Value
  , _invalidOffendingData    :: !P.Pointer
  } deriving (Eq, Show)

setFailure :: b -> Failure a -> Failure b
setFailure e (Invalid _ a b) = Invalid e a b

modFailure :: (a -> b) -> Failure a -> Failure b
modFailure f v@(Invalid a _ _) = v { _invalidValidatorsCalled = f a }

addToPath :: P.Token -> Failure a -> Failure a
addToPath tok v@(Invalid _ _ a) = v { _invalidOffendingData = addToken a }
  where
    addToken :: P.Pointer -> P.Pointer
    addToken (P.Pointer ts) = P.Pointer (ts <> pure tok)
