
module JSONSchema.Draft4.Spec where

import           Import

import           Data.Maybe                     (fromMaybe)
import           Data.Profunctor                (Profunctor(..))

import           JSONSchema.Draft4.Failure
import           JSONSchema.Draft4.Schema       (Schema(..),
                                                 emptySchema)
import           JSONSchema.Fetch               (SchemaWithURI(..),
                                                 URISchemaMap(..))
import qualified JSONSchema.Fetch               as FE
import           JSONSchema.Types               (Spec(..))
import qualified JSONSchema.Types               as JT
import           JSONSchema.Validator.Draft4
import           JSONSchema.Validator.Reference (BaseURI(..),
                                                 Scope(..),
                                                 updateResolutionScope)

-- | An implementation of 'JT.embedded'.
embedded :: Schema -> ([Schema], [Schema])
embedded s =
    JT.embedded (d4Spec mempty mempty (Scope s Nothing (BaseURI Nothing))) s

specValidate
    :: URISchemaMap Schema
    -> SchemaWithURI Schema
    -> Value
    -> [ValidatorFailure]
specValidate schemaMap sw =
    JT.validate (d4Spec schemaMap visited scope) (_swSchema sw)
  where
    visited :: VisitedSchemas
    visited = VisitedSchemas [(Nothing, Nothing)]

    scope :: Scope Schema
    scope = Scope
        { _topLevelDocument = _swSchema sw
        , _documentURI      = _swURI sw
        , _currentBaseURI   = updateResolutionScope (BaseURI (_swURI sw))
                                                    (_schemaId (_swSchema sw))
        }

validateSubschema 
    :: URISchemaMap Schema
    -> VisitedSchemas
    -> Scope Schema
    -> Schema
    -> Value
    -> [ValidatorFailure]
validateSubschema schemaMap visited scope schema =
    JT.validate (d4Spec schemaMap visited newScope) schema
  where
    newScope :: Scope Schema
    newScope = scope
        { _currentBaseURI = updateResolutionScope (_currentBaseURI scope)
                                                  (_schemaId schema)
        }

d4Spec
    :: URISchemaMap Schema
    -> VisitedSchemas
    -> Scope Schema
    -> Spec Schema ValidatorFailure
d4Spec schemaMap visited scope = Spec $
    [ dimap
        (fmap Ref . _schemaRef)
        FailureRef
        (refValidator (FE.getReference schemaMap) updateScope valRef visited scope)
    ]

    <> fmap (lmap disableIfRefPresent)
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

    , dimap (fmap EnumValidator . _schemaEnum) FailureEnum enumValidator
    , dimap (fmap TypeContext . _schemaType) FailureType typeValidator
    , dimap (fmap AllOf . _schemaAllOf) FailureAllOf (allOfValidator lateral)
    , dimap (fmap AnyOf . _schemaAnyOf) FailureAnyOf (anyOfValidator lateral)
    , dimap (fmap OneOf . _schemaOneOf) FailureOneOf (oneOfValidator lateral)
    , dimap (fmap NotValidator . _schemaNot) FailureNot (notValidator lateral)
    ]
  where
    disableIfRefPresent :: Schema -> Schema
    disableIfRefPresent schema =
        case _schemaRef schema of
            Nothing -> schema
            Just _  -> emptySchema

    updateScope :: BaseURI -> Schema -> BaseURI
    updateScope uri schema = updateResolutionScope uri (_schemaId schema)

    valRef
        :: VisitedSchemas
        -> Scope Schema
        -> Schema
        -> Value
        -> [ValidatorFailure]
    valRef vis sc = JT.validate (d4Spec schemaMap vis sc)

    descend :: Schema -> Value -> [ValidatorFailure]
    descend = validateSubschema schemaMap mempty scope

    lateral :: Schema -> Value -> [ValidatorFailure]
    lateral = validateSubschema schemaMap visited scope
