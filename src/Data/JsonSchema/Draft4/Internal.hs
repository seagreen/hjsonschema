
module Data.JsonSchema.Draft4.Internal where

import           Data.Aeson
import qualified Data.HashMap.Strict            as H
import qualified Data.List.NonEmpty             as N
import           Data.Maybe                     (catMaybes, fromMaybe, isJust,
                                                 maybeToList)
import           Data.Scientific

import           Data.JsonSchema.Draft4.Failure
import           Data.JsonSchema.Draft4.Schema
import           Data.JsonSchema.Fetch          (ReferencedSchemas(..),
                                                 SchemaWithURI(..))
import qualified Data.Validator.Draft4.Any      as AN
import qualified Data.Validator.Draft4.Array    as AR
import qualified Data.Validator.Draft4.Number   as NU
import qualified Data.Validator.Draft4.Object   as OB
import qualified Data.Validator.Draft4.String   as ST
import           Data.Validator.Failure         (modFailure, setFailure)
import qualified Data.Validator.Failure         as FR
import           Data.Validator.Reference       (updateResolutionScope)
import           Import

-- For GHCs before 7.10:
import           Prelude                        hiding (concat)

--------------------------------------------------
-- * Embedded Schemas
--------------------------------------------------

-- | Return a schema's immediate subschemas.
--
-- Pass this to 'fetchReferencedSchemas' so that function can find all the
-- subschemas in a document. This allows 'fetchReferencedSchemas' to process
-- only "$ref"s and "id"s that are actual schema keywords. For example,
-- within a "properties" validator object an "id" key doesn't actually change
-- any scope, but instead serves a validator-specific function.
embedded :: Schema -> [Schema]
embedded schema = concat
  [ f _schemaItems
      (\x -> case x of
               AR.ItemsObject s -> pure s
               AR.ItemsArray ss -> ss
      )
  , f _schemaAdditionalItems
      (\x -> case x of
               AR.AdditionalObject s -> pure s
               _                     -> mempty
      )
  , f _schemaDependencies (catMaybes . fmap checkDependency . H.elems)
  , f _schemaProperties H.elems
  , f _schemaPatternProperties H.elems
  , f _schemaAdditionalProperties
      (\x -> case x of
               OB.AdditionalPropertiesObject s -> pure s
               _                               -> mempty
      )
  , f _schemaAllOf N.toList
  , f _schemaAnyOf N.toList
  , f _schemaOneOf N.toList
  , f _schemaNot pure
  , f _schemaDefinitions H.elems
  ]
  where
    f :: (Schema -> Maybe a) -> (a -> [Schema]) -> [Schema]
    f field nextLevelEmbedded = maybe mempty nextLevelEmbedded (field schema)

    checkDependency :: OB.Dependency Schema -> Maybe Schema
    checkDependency (OB.PropertyDependency _) = Nothing
    checkDependency (OB.SchemaDependency s)   = Just s

--------------------------------------------------
-- * Validation (Main internal functions)
--------------------------------------------------

runValidate
  :: ReferencedSchemas Schema
  -> SchemaWithURI Schema
  -> Value
  -> [Invalid]
runValidate referenced sw x = concat
  [ f _schemaEnum  (setFailure Enum)          (fmap maybeToList . AN.enumVal)
  , f _schemaType  (setFailure TypeValidator) (fmap maybeToList . AN.typeVal)
  , f _schemaAllOf (modFailure AllOf)         (AN.allOf recurse)
  , f _schemaAnyOf (setFailure AnyOf)         (fmap maybeToList . AN.anyOf recurse)
  , f _schemaOneOf (setFailure OneOf)         (fmap maybeToList . AN.oneOf recurse)
  , f _schemaNot   (setFailure NotValidator)  (fmap maybeToList . AN.notVal recurse)
  , refFailures
  ] <> specificValidators
  where
    specificValidators :: [Invalid]
    specificValidators =
      case x of
        Number y -> validateNumber (_swSchema sw) y
        String y -> validateString (_swSchema sw) y
        Array y  -> validateArray referenced sw y
        Object y -> validateObject referenced sw y
        _        -> mempty

    f = runSingle (_swSchema sw) x

    recurse = descendNextLevel referenced sw

    -- Since the results of the 'AN.ref' validator are fairly complicated [1]
    -- it's simpler not to use our 'f' helper function for it.
    --
    -- [1] A list of errors wrapped in a 'Maybe' where 'Nothing' represents
    -- if resolving the reference itself failed.
    refFailures :: [Invalid]
    refFailures =
      case _schemaRef (_swSchema sw) of
        Nothing        -> mempty
        Just reference ->
          maybe [FR.Invalid RefResolution (toJSON reference) mempty]
                (fmap (modFailure Ref))
                $ AN.ref scope
                         getReference
                         (\a b -> runValidate referenced (SchemaWithURI b a))
                         reference
                         x
      where
        scope :: Maybe Text
        scope = updateResolutionScope (_swURI sw) (_schemaId (_swSchema sw))

        getReference :: Maybe Text -> Maybe Schema
        getReference Nothing  = Just (_rsStarting referenced)
        getReference (Just t) = H.lookup t (_rsSchemaMap referenced)

validateString
  :: Schema
  -> Text
  -> [Invalid]
validateString schema x = concat
  [ f _schemaMaxLength (setFailure MaxLength)        (fmap maybeToList . ST.maxLength)
  , f _schemaMinLength (setFailure MinLength)        (fmap maybeToList . ST.minLength)
  , f _schemaPattern   (setFailure PatternValidator) (fmap maybeToList . ST.patternVal)
  ]
  where
    f = runSingle schema x

validateNumber
  :: Schema
  -> Scientific
  -> [Invalid]
validateNumber schema x = concat
  [ f _schemaMultipleOf (setFailure MultipleOf) (fmap maybeToList . NU.multipleOf)
  , f _schemaMaximum
      (modFailure fMax)
      ( fmap maybeToList
      . NU.maximumVal (fromMaybe False (_schemaExclusiveMaximum schema))
      )
  , f _schemaMinimum
      (modFailure fMin)
      ( fmap maybeToList
      . NU.minimumVal (fromMaybe False (_schemaExclusiveMinimum schema))
      )
  ]
  where
    f = runSingle schema x

    fMax NU.Maximum          = Maximum
    fMax NU.ExclusiveMaximum = ExclusiveMaximum

    fMin NU.Minimum          = Minimum
    fMin NU.ExclusiveMinimum = ExclusiveMinimum

validateArray
  :: ReferencedSchemas Schema
  -> SchemaWithURI Schema
  -> Vector Value
  -> [Invalid]
validateArray referenced (SchemaWithURI schema mUri) x = concat
  [ f _schemaMaxItems    (setFailure MaxItems)    (fmap maybeToList . AR.maxItems)
  , f _schemaMinItems    (setFailure MinItems)    (fmap maybeToList . AR.minItems)
  , f _schemaUniqueItems (setFailure UniqueItems) (fmap maybeToList . AR.uniqueItems)
  , f _schemaItems
      (modFailure fItems)
      (AR.items recurse (_schemaAdditionalItems schema))
  ]
  where
    f = runSingle schema x

    recurse = descendNextLevel referenced (SchemaWithURI schema mUri)

    fItems (AR.Items err)                        = Items err
    fItems AR.AdditionalItemsBoolInvalid         = AdditionalItemsBool
    fItems (AR.AdditionalItemsObjectInvalid err) = AdditionalItemsObject err

validateObject
  :: ReferencedSchemas Schema
  -> SchemaWithURI Schema
  -> HashMap Text Value
  -> [Invalid]
validateObject referenced (SchemaWithURI schema mUri) x = concat
  [ f _schemaMaxProperties (setFailure MaxProperties) (fmap maybeToList . OB.maxProperties)
  , f _schemaMinProperties (setFailure MinProperties) (fmap maybeToList . OB.minProperties)
  , f _schemaRequired      (setFailure Required)      (fmap maybeToList . OB.required)
  , f _schemaDependencies  (modFailure fDeps)         (OB.dependencies recurse)

  , f _schemaProperties
      (modFailure fProp)
      (OB.properties recurse
                     (_schemaPatternProperties schema)
                     (_schemaAdditionalProperties schema))

  , f _schemaPatternProperties
      (modFailure fPatProp)
      (case _schemaProperties schema of
        Just _  -> const (const mempty)
        Nothing -> OB.patternProperties recurse (_schemaAdditionalProperties schema))

  , f _schemaAdditionalProperties
      (modFailure fAddProp)
      (if isJust (_schemaProperties schema) || isJust (_schemaPatternProperties schema)
        then const (const mempty)
        else OB.additionalProperties recurse)
  ]
  where
    f = runSingle schema x

    recurse = descendNextLevel referenced (SchemaWithURI schema mUri)

    fDeps (OB.SchemaDependencyInvalid err) = SchemaDependency err
    fDeps OB.PropertyDependencyInvalid     = PropertyDependency

    fProp (OB.PropertiesInvalid err)   = Properties err
    fProp (OB.PropPatternInvalid err)  = PatternProperties err
    fProp (OB.PropAdditionalInvalid a) =
      case a of
        OB.APBoolInvalid       -> AdditionalPropertiesBool
        OB.APObjectInvalid err -> AdditionalPropertiesObject err

    fPatProp (OB.PPInvalid err) = PatternProperties err
    fPatProp (OB.PPAdditionalPropertiesInvalid a)   =
      case a of
        OB.APBoolInvalid       -> AdditionalPropertiesBool
        OB.APObjectInvalid err -> AdditionalPropertiesObject err

    fAddProp OB.APBoolInvalid         = AdditionalPropertiesBool
    fAddProp (OB.APObjectInvalid err) = AdditionalItemsObject err

--------------------------------------------------
-- * Validation (Internal utils)
--------------------------------------------------

descendNextLevel
  :: ReferencedSchemas Schema
  -> SchemaWithURI Schema
  -> Schema
  -> Value
  -> [Invalid]
descendNextLevel referenced (SchemaWithURI schema mUri) =
  runValidate referenced . flip SchemaWithURI scope
  where
    scope :: Maybe Text
    scope = updateResolutionScope mUri (_schemaId schema)

runSingle
  :: Schema
  -> dta
  -> (Schema -> Maybe val)
  -> (err -> Invalid)
  -> (val -> dta -> [err])
  -> [Invalid]
runSingle schema dta field modifyError validate =
  maybe mempty (\val -> modifyError <$> validate val dta) (field schema)
