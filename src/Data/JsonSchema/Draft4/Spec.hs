
module Data.JsonSchema.Draft4.Spec where

import           Import

import           Data.Maybe                     (fromMaybe)
import           Data.Profunctor                (Profunctor(..))

import           Data.JsonSchema.Draft4.Failure
import           Data.JsonSchema.Draft4.Schema  (Schema(..))
import           Data.JsonSchema.Fetch          (ReferencedSchemas(..),
                                                 SchemaWithURI(..))
import qualified Data.JsonSchema.Fetch          as FE
import           Data.JsonSchema.Types          (Spec(..))
import qualified Data.JsonSchema.Types          as JT
import           Data.Validator.Draft4
import           Data.Validator.Reference       (updateResolutionScope)

embedded :: Schema -> ([Schema], [Schema])
embedded s = JT.embedded (d4Spec (ReferencedSchemas s mempty) mempty Nothing) s

specValidate
    :: ReferencedSchemas Schema
    -> SchemaWithURI Schema
    -> Value
    -> [ValidatorFailure]
specValidate rs =
    continueValidating rs (VisitedSchemas [(Nothing, Nothing)])

continueValidating
    :: ReferencedSchemas Schema
    -> VisitedSchemas
    -> SchemaWithURI Schema
    -> Value
    -> [ValidatorFailure]
continueValidating referenced visited sw =
    JT.validate (d4Spec referenced visited currentScope)
                (_swSchema sw)
  where
    currentScope :: Maybe Text
    currentScope = updateResolutionScope
                       (_swURI sw)
                       (_schemaId (_swSchema sw))

d4Spec
    :: ReferencedSchemas Schema
    -> VisitedSchemas
    -> Maybe Text
    -> Spec Schema ValidatorFailure
d4Spec referenced visited scope = Spec
    [ dimap (fmap MultipleOf . _schemaMultipleOf) FailureMultipleOf multipleOfValidator
    , dimap
        (\s -> Maximum (fromMaybe False (_schemaExclusiveMaximum s)) <$> _schemaMaximum s)
        FailureMaximum
        maximumValidator
    , dimap
        (\s -> Minimum (fromMaybe False (_schemaExclusiveMinimum s)) <$> _schemaMinimum s)
        FailureMinimum
        minimumValidator

    , dimap (fmap MaxLength . _schemaMaxLength) FailureMaxLength maxLengthValidator
    , dimap (fmap MinLength . _schemaMinLength) FailureMinLength minLengthValidator
    , dimap (fmap PatternValidator . _schemaPattern) FailurePattern patternValidator

    , dimap (fmap MaxItems . _schemaMaxItems) FailureMaxItems maxItemsValidator
    , dimap (fmap MinItems . _schemaMinItems) FailureMinItems minItemsValidator
    , dimap (fmap UniqueItems . _schemaUniqueItems) FailureUniqueItems uniqueItemsValidator
    , dimap
        (\s -> ItemsRelated
                   { _irItems      = _schemaItems s
                   , _irAdditional = _schemaAdditionalItems s
                   })
        (\err -> case err of
                     IRInvalidItems e      -> FailureItems e
                     IRInvalidAdditional e -> FailureAdditionalItems e)
        (itemsRelatedValidator descend)
    , lmap (fmap Definitions . _schemaDefinitions) definitionsEmbedded

    , dimap
        (fmap MaxProperties . _schemaMaxProperties)
        FailureMaxProperties
        maxPropertiesValidator
    , dimap
        (fmap MinProperties . _schemaMinProperties)
        FailureMinProperties
        minPropertiesValidator
    , dimap (fmap Required . _schemaRequired) FailureRequired requiredValidator
    , dimap
        (fmap DependenciesValidator . _schemaDependencies)
        FailureDependencies
        (dependenciesValidator descend)
    , dimap
        (\s -> PropertiesRelated
                   { _propProperties = _schemaProperties s
                   , _propPattern    = _schemaPatternProperties s
                   , _propAdditional = _schemaAdditionalProperties s
                   })
        FailurePropertiesRelated
        (propertiesRelatedValidator descend)

    , dimap
        (\s -> Ref <$> _schemaRef s)
        FailureRef
        (refValidator visited scope (FE.getReference referenced) getRef)
    , dimap (fmap EnumValidator . _schemaEnum) FailureEnum enumValidator
    , dimap (fmap TypeContext . _schemaType) FailureType typeValidator
    , dimap (fmap AllOf . _schemaAllOf) FailureAllOf (allOfValidator lateral)
    , dimap (fmap AnyOf . _schemaAnyOf) FailureAnyOf (anyOfValidator lateral)
    , dimap (fmap OneOf . _schemaOneOf) FailureOneOf (oneOfValidator lateral)
    , dimap (fmap NotValidator . _schemaNot) FailureNot (notValidator lateral)
    ]
  where
    getRef
        :: VisitedSchemas
        -> Maybe Text
        -> Schema
        -> Value
        -> [ValidatorFailure]
    getRef newVisited newScope schema =
        continueValidating referenced newVisited (SchemaWithURI schema newScope)

    descend :: Schema -> Value -> [ValidatorFailure]
    descend schema =
        continueValidating referenced mempty (SchemaWithURI schema scope)

    lateral :: Schema -> Value -> [ValidatorFailure]
    lateral schema =
        continueValidating referenced visited (SchemaWithURI schema scope)
