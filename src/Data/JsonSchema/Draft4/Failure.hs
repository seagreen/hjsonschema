
module Data.JsonSchema.Draft4.Failure where

import           Prelude

import           Data.List.NonEmpty            (NonEmpty)
import           Data.Text                     (Text)

import qualified Data.Validator.Draft4.Any    as AN
import qualified Data.Validator.Draft4.Array  as AR
import qualified Data.Validator.Draft4.Number as NU
import qualified Data.Validator.Draft4.Object as OB
import qualified Data.Validator.Failure       as FR

-- | A description of why a schema (or one of its reference) is itself
-- invalid.
--
-- 'Nothing' indicates the starting schema. 'Just' indicates a referenced
-- schema -- the contents of the 'Just' is the schema's URI.
--
-- NOTE: 'HashMap (Maybe Text) Invalid' would be a nicer way of defining
-- this, but then we lose the guarantee that there's at least one key.
type InvalidSchema = NonEmpty (Maybe Text, Failure)

type Invalid = NonEmpty Failure

type Failure = FR.Fail ValidatorChain

-- | Distinguish all the different possible causes of failure for
-- Draft 4 validation.
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
    | RefLoop
    | Ref ValidatorChain
    | Enum
    | TypeValidator
    | AllOf ValidatorChain
    | AnyOf ValidatorChain
    | OneOfTooManySuccesses
    | OneOfNoSuccesses ValidatorChain
    | NotValidator
    deriving (Eq, Show)

maxE :: NU.MaximumInvalid -> ValidatorChain
maxE NU.Maximum          = Maximum
maxE NU.ExclusiveMaximum = ExclusiveMaximum

minE :: NU.MinimumInvalid -> ValidatorChain
minE NU.Minimum          = Minimum
minE NU.ExclusiveMinimum = ExclusiveMinimum

itemsE :: AR.ItemsInvalid ValidatorChain -> ValidatorChain
itemsE (AR.Items err)                        = Items err
itemsE AR.AdditionalItemsBoolInvalid         = AdditionalItemsBool
itemsE (AR.AdditionalItemsObjectInvalid err) = AdditionalItemsObject err

depsE :: OB.DependencyInvalid ValidatorChain -> ValidatorChain
depsE (OB.SchemaDependencyInvalid err) = SchemaDependency err
depsE OB.PropertyDependencyInvalid     = PropertyDependency

propE :: OB.PropertiesInvalid ValidatorChain -> ValidatorChain
propE (OB.PropertiesInvalid err)   = Properties err
propE (OB.PropPatternInvalid err)  = PatternProperties err
propE (OB.PropAdditionalInvalid a) =
    case a of
        OB.APBoolInvalid       -> AdditionalPropertiesBool
        OB.APObjectInvalid err -> AdditionalPropertiesObject err

patPropE :: OB.PatternPropertiesInvalid ValidatorChain -> ValidatorChain
patPropE (OB.PPInvalid err) = PatternProperties err
patPropE (OB.PPAdditionalPropertiesInvalid a)   =
    case a of
        OB.APBoolInvalid       -> AdditionalPropertiesBool
        OB.APObjectInvalid err -> AdditionalPropertiesObject err

addPropE :: OB.AdditionalPropertiesInvalid ValidatorChain -> ValidatorChain
addPropE OB.APBoolInvalid         = AdditionalPropertiesBool
addPropE (OB.APObjectInvalid err) = AdditionalPropertiesObject err

refE :: AN.RefInvalid ValidatorChain -> ValidatorChain
refE AN.RefResolution    = RefResolution
refE AN.RefLoop          = RefLoop
refE (AN.RefInvalid err) = Ref err

oneOfE :: AN.OneOfInvalid ValidatorChain -> ValidatorChain
oneOfE AN.TooManySuccesses  = OneOfTooManySuccesses
oneOfE (AN.NoSuccesses err) = OneOfNoSuccesses err
