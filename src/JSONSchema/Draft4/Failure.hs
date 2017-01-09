
module JSONSchema.Draft4.Failure where

import           Import

import           JSONSchema.Draft4.Schema    (Schema)
import qualified JSONSchema.Validator.Draft4 as VAL

-- | Used to report an entire instance being invalidated, as opposed
-- to the failure of a single validator.
data Invalid = Invalid
    { _invalidSchema   :: Schema
    , _invalidInstance :: Value
    , _invalidFailures :: NonEmpty ValidatorFailure
    } deriving (Eq, Show)

data ValidatorFailure
    = FailureMultipleOf VAL.MultipleOfInvalid
    | FailureMaximum    VAL.MaximumInvalid
    | FailureMinimum    VAL.MinimumInvalid

    | FailureMaxLength VAL.MaxLengthInvalid
    | FailureMinLength VAL.MinLengthInvalid
    | FailurePattern   VAL.PatternInvalid

    | FailureMaxItems        VAL.MaxItemsInvalid
    | FailureMinItems        VAL.MinItemsInvalid
    | FailureUniqueItems     VAL.UniqueItemsInvalid
    | FailureItems           (VAL.ItemsInvalid ValidatorFailure)
    | FailureAdditionalItems (VAL.AdditionalItemsInvalid ValidatorFailure)

    | FailureMaxProperties     VAL.MaxPropertiesInvalid
    | FailureMinProperties     VAL.MinPropertiesInvalid
    | FailureRequired          VAL.RequiredInvalid
    | FailureDependencies      (VAL.DependenciesInvalid ValidatorFailure)
    | FailurePropertiesRelated (VAL.PropertiesRelatedInvalid ValidatorFailure)

    | FailureRef   (VAL.RefInvalid ValidatorFailure)
    | FailureEnum  VAL.EnumInvalid
    | FailureType  VAL.TypeValidatorInvalid
    | FailureAllOf (VAL.AllOfInvalid ValidatorFailure)
    | FailureAnyOf (VAL.AnyOfInvalid ValidatorFailure)
    | FailureOneOf (VAL.OneOfInvalid ValidatorFailure)
    | FailureNot   VAL.NotValidatorInvalid
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
