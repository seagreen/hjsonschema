
module Data.JsonSchema.Draft4.Failure where

import           Import

import           Data.JsonSchema.Draft4.Schema (Schema)
import qualified Data.Validator.Draft4         as D4

-- | Used to report an entire instance being invalidated, as opposed
-- to the failure of a single validator.
data Invalid = Invalid
    { _invalidSchema   :: Schema
    , _invalidInstance :: Value
    , _invalidFailures :: NonEmpty ValidatorFailure
    } deriving (Eq, Show)

data ValidatorFailure
    = FailureMultipleOf D4.MultipleOfInvalid
    | FailureMaximum    D4.MaximumInvalid
    | FailureMinimum    D4.MinimumInvalid

    | FailureMaxLength D4.MaxLengthInvalid
    | FailureMinLength D4.MinLengthInvalid
    | FailurePattern   D4.PatternInvalid

    | FailureMaxItems        D4.MaxItemsInvalid
    | FailureMinItems        D4.MinItemsInvalid
    | FailureUniqueItems     D4.UniqueItemsInvalid
    | FailureItems           (D4.ItemsInvalid ValidatorFailure)
    | FailureAdditionalItems (D4.AdditionalItemsInvalid ValidatorFailure)

    | FailureMaxProperties     D4.MaxPropertiesInvalid
    | FailureMinProperties     D4.MinPropertiesInvalid
    | FailureRequired          ()
    | FailureDependencies      (D4.DependenciesInvalid ValidatorFailure)
    | FailurePropertiesRelated (D4.PropertiesRelatedInvalid ValidatorFailure)

    | FailureRef   (D4.RefInvalid ValidatorFailure)
    | FailureEnum  D4.EnumInvalid
    | FailureType  D4.TypeValidatorInvalid
    | FailureAllOf (D4.AllOfInvalid ValidatorFailure)
    | FailureAnyOf (D4.AnyOfInvalid ValidatorFailure)
    | FailureOneOf (D4.OneOfInvalid ValidatorFailure)
    | FailureNot   D4.NotValidatorInvalid
    deriving (Eq, Show)

-- | A description of why a schema (or one of its reference) is itself
-- invalid.
--
-- 'Nothing' indicates the starting schema. 'Just' indicates a referenced
-- schema. The contents of the 'Just' is the schema's URI.
--
-- NOTE: 'HashMap (Maybe Text) Invalid' would be a nicer way of
-- defining this, but then we lose the guarantee that there's at least
-- one key.
newtype SchemaInvalid
    = SchemaInvalid {
        _unSchemaInvalid :: NonEmpty (Maybe Text, NonEmpty ValidatorFailure) }
    deriving (Eq, Show)
