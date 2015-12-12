
module Data.JsonSchema.Draft4.Failure where

import qualified Data.Aeson.Pointer     as P

import qualified Data.Validator.Failure as FR
import           Import

-- | A version of 'FR.Failure' specialized for JSON Schema Draft 4.
data Failure = Failure
  -- NOTE: We use a data type here instead of a newtype to provide a cleaner
  -- API for those importing only 'Data.JsonSchema.Draft4' (which should be
  -- the vast majority of users). Downsides: slower and hacky. I'd appreciate
  -- feedback on this.
  { _failureValidatorsCalled :: !ValidatorChain
  , _failureFinalValidator   :: !Value
  , _failureOffendingData    :: !P.Pointer
  } deriving (Eq, Show)

specializeForDraft4 :: FR.Failure ValidatorChain -> Failure
specializeForDraft4 (FR.Failure a b c) = Failure a b c

data ValidatorChain
  = MultipleOf
  | Maximum
  | ExclusiveMaximum
  | Minimum
  | ExclusiveMinimum

  | MaxLength
  | MinLength
  | PatternValidator

  | MaxItems
  | MinItems
  | UniqueItems
  | Items ValidatorChain
  | AdditionalItemsBool
  | AdditionalItemsObject ValidatorChain

  | MaxProperties
  | MinProperties
  | Required
  | SchemaDependency ValidatorChain
  | PropertyDependency
  | Properties ValidatorChain
  | PatternProperties ValidatorChain
  | AdditionalPropertiesBool
  | AdditionalPropertiesObject ValidatorChain

  | RefResolution
    -- ^ Indicates a reference that failed to resolve.
    --
    -- NOTE: The language agnostic test suite doesn't specify if this should
    -- cause a validation error or should allow data to pass. We choose to
    -- return a validation error.
    --
    -- Also note that ideally we would enforce in the type system that any
    -- failing references be dealt with before valididation. Then this could
    -- be removed entirely.
  | Ref ValidatorChain
  | Enum
  | TypeValidator
  | AllOf ValidatorChain
  | AnyOf
  | OneOf
  | NotValidator
  deriving (Eq, Show)
