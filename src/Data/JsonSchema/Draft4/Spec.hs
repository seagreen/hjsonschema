
module Data.JsonSchema.Draft4.Spec where

import           Import
-- Hiding is for GHCs before 7.10:
import           Prelude                        hiding (concat)

import           Data.Maybe                     (fromMaybe, isNothing)
import           Data.Profunctor                (Profunctor(..))

import           Data.JsonSchema.Draft4.Failure
import           Data.JsonSchema.Draft4.Schema  (Schema(..))
import           Data.JsonSchema.Fetch          (ReferencedSchemas(..),
                                                 SchemaWithURI(..))
import qualified Data.JsonSchema.Fetch          as FE
import           Data.JsonSchema.Types          (Spec(..))
import qualified Data.JsonSchema.Types          as JT
import qualified Data.Validator.Draft4          as D4
import qualified Data.Validator.Draft4.Any      as AN
import           Data.Validator.Reference       (updateResolutionScope)

embedded :: Schema -> ([Schema], [Schema])
embedded s = JT.embedded (d4Spec (ReferencedSchemas s mempty) mempty Nothing) s

validate
    :: ReferencedSchemas Schema
    -> SchemaWithURI Schema
    -> Value
    -> [Failure]
validate rs = continueValidating rs (AN.VisitedSchemas [(Nothing, Nothing)])

continueValidating
    :: ReferencedSchemas Schema
    -> AN.VisitedSchemas
    -> SchemaWithURI Schema
    -> Value
    -> [Failure]
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
    -> AN.VisitedSchemas
    -> Maybe Text
    -> Spec Schema ValidatorChain
d4Spec referenced visited scope = Spec
    [ dimap
        (fmap D4.MultipleOf . _schemaMultipleOf)
        (const MultipleOf)
        D4.multipleOf
    , dimap
        (\s -> D4.MaximumContext (fromMaybe False (_schemaExclusiveMaximum s))
                 <$> _schemaMaximum s)
        maxE
        D4.maximumVal
    , dimap
        (\s -> D4.MinimumContext (fromMaybe False (_schemaExclusiveMinimum s))
                 <$> _schemaMinimum s)
        minE
        D4.minimumVal

    , dimap (fmap D4.MaxLength . _schemaMaxLength) (const MaxLength) D4.maxLength
    , dimap (fmap D4.MinLength . _schemaMinLength) (const MinLength) D4.minLength
    , dimap (fmap D4.PatternVal . _schemaPattern) (const PatternValidator) D4.patternVal

    , dimap (fmap D4.MaxItems . _schemaMaxItems) (const MaxItems) D4.maxItems
    , dimap (fmap D4.MinItems . _schemaMinItems) (const MinItems) D4.minItems
    , dimap (fmap D4.UniqueItems . _schemaUniqueItems) (const UniqueItems) D4.uniqueItems
    , dimap
        (\s -> D4.ItemsContext (_schemaAdditionalItems s) <$> _schemaItems s)
        itemsE
        (D4.items descend)
    , lmap (fmap D4.AdditionalItemsContext . _schemaAdditionalItems) D4.additionalItemsEmbedded
    , lmap (fmap D4.Definitions . _schemaDefinitions) D4.definitionsEmbedded

    , dimap (fmap D4.MaxProperties . _schemaMaxProperties) (const MaxProperties) D4.maxProperties
    , dimap (fmap D4.MinProperties . _schemaMinProperties) (const MinProperties) D4.minProperties
    , dimap (fmap D4.RequiredContext . _schemaRequired) (const Required) D4.required
    , dimap (fmap D4.DependenciesContext . _schemaDependencies) depsE (D4.dependencies descend)
    , dimap
        (\s -> D4.PropertiesContext
                 (_schemaPatternProperties s)
                 (_schemaAdditionalProperties s)
                 <$> _schemaProperties s)
        propE
        (D4.properties descend)
    , dimap
        (\s -> D4.PatternPropertiesContext
                 (isNothing (_schemaProperties s))
                 (_schemaAdditionalProperties s)
                 <$> _schemaPatternProperties s)
        patPropE
        (D4.patternProperties descend)
    , dimap
        (\s -> D4.AdditionalPropertiesContext
                 (isNothing (_schemaProperties s)
                    && isNothing (_schemaPatternProperties s))
                 <$> _schemaAdditionalProperties s)
        addPropE
        (D4.additionalProperties descend)

    , dimap
        (\s -> D4.Ref <$> _schemaRef s)
        refE
        (D4.ref visited scope (FE.getReference referenced) refVal)
    , dimap (fmap D4.EnumContext . _schemaEnum) (const Enum) D4.enumVal
    , dimap (fmap D4.TypeContext . _schemaType) (const TypeValidator) D4.typeVal
    , dimap (fmap D4.AllOf . _schemaAllOf) AllOf (D4.allOf lateral)
    , dimap (fmap D4.AnyOf . _schemaAnyOf) (const AnyOf) (D4.anyOf lateral)
    , dimap (fmap D4.OneOf . _schemaOneOf) (const OneOf) (D4.oneOf lateral)
    , dimap (fmap D4.NotVal . _schemaNot) (const NotValidator) (D4.notVal lateral)
    ]
  where
    refVal :: AN.VisitedSchemas -> Maybe Text -> Schema -> Value -> [Failure]
    refVal newVisited newScope schema =
        continueValidating referenced newVisited (SchemaWithURI schema newScope)

    descend :: Schema -> Value -> [Failure]
    descend schema =
        continueValidating referenced mempty (SchemaWithURI schema scope)

    lateral :: Schema -> Value -> [Failure]
    lateral schema =
        continueValidating referenced visited (SchemaWithURI schema scope)
