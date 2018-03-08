-- | An implementation of JSON Schema Draft 4 based on 'HashMap Text Value'
-- instead of a custom record type like 'JSONSchema.Draft4'.
--
-- If you're writing code for a new schema specification you probably want
-- to copy this module instead of the 'JSONSchema.Draft4'. While it's
-- less convenient to write schemas in Haskell without a record type, you
-- can get the implementation finished with far fewer lines of code.
module AlternateSchema where

import           Protolude

import           Data.Aeson (FromJSON(..), Value(..), decodeStrict)
import qualified Data.Aeson as AE
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import           Data.Profunctor (Profunctor(..))

import           JSONSchema.Draft4 (ValidatorFailure(..), metaSchemaBytes)
import           JSONSchema.Fetch (SchemaWithURI(..), URISchemaMap(..))
import qualified JSONSchema.Fetch as FE
import           JSONSchema.Types (Schema(..), Spec(..))
import qualified JSONSchema.Types as JT
import qualified JSONSchema.Validator.Draft4 as D4
import           JSONSchema.Validator.Reference (BaseURI(..), Scope(..),
                                                 updateResolutionScope)

--------------------------------------------------
-- * Basic fetching tools
--------------------------------------------------

referencesViaHTTP
    :: SchemaWithURI Schema
    -> IO (Either FE.HTTPFailure (FE.URISchemaMap Schema))
referencesViaHTTP = FE.referencesViaHTTP' draft4FetchInfo

draft4FetchInfo :: FE.FetchInfo Schema
draft4FetchInfo = FE.FetchInfo embedded (lookup "id") (lookup "$ref")
  where
    lookup :: Text -> Schema -> Maybe Text
    lookup k (Schema s) =
        case HM.lookup k s of
            Just (String t) -> Just t
            _               -> Nothing

-- | An implementation of 'JT.embedded'.
embedded :: Schema -> ([Schema], [Schema])
embedded s =
    JT.embedded (d4Spec mempty mempty (Scope s Nothing (BaseURI Nothing))) s

--------------------------------------------------
-- * Main API
--------------------------------------------------

validate
    :: URISchemaMap Schema
    -> SchemaWithURI Schema
    -> Value
    -> [ValidatorFailure]
validate schemaMap sw =
    JT.validate (d4Spec schemaMap visited scope) (_swSchema sw)
  where
    visited :: D4.VisitedSchemas
    visited = D4.VisitedSchemas [(Nothing, Nothing)]

    schemaId :: Maybe Text
    schemaId = FE._fiId draft4FetchInfo (_swSchema sw)

    scope :: Scope Schema
    scope = Scope
        { _topLevelDocument = _swSchema sw
        , _documentURI      = _swURI sw
        , _currentBaseURI   =  updateResolutionScope (BaseURI (_swURI sw))
                                                     schemaId
        }

-- | A schema for schemas themselves. Uses @src/draft4.json@ which is loaded
-- at compile time.
metaSchema :: Schema
metaSchema =
      fromMaybe (panic "Schema decode failed (this should never happen)")
    . decodeStrict
    $ metaSchemaBytes

checkSchema :: Schema -> [ValidatorFailure]
checkSchema = validate schemaMap (SchemaWithURI metaSchema Nothing)
            . Object
            . _unSchema
  where
    schemaMap :: URISchemaMap Schema
    schemaMap =
        URISchemaMap (HM.singleton "http://json-schema.org/draft-04/schema"
                                   metaSchema)

--------------------------------------------------
-- * Spec
--------------------------------------------------

validateSubschema
    :: URISchemaMap Schema
    -> D4.VisitedSchemas
    -> Scope Schema
    -> Schema
    -> Value
    -> [ValidatorFailure]
validateSubschema schemaMap visited scope schema =
    JT.validate (d4Spec schemaMap visited newScope) schema
  where
    schemaId :: Maybe Text
    schemaId = FE._fiId draft4FetchInfo schema

    newScope :: Scope Schema
    newScope = scope
        { _currentBaseURI = updateResolutionScope (_currentBaseURI scope)
                                                  schemaId
        }

d4Spec
    :: URISchemaMap Schema
    -> D4.VisitedSchemas
    -> Scope Schema
    -> Spec Schema ValidatorFailure
       -- ^ Here we reuses 'ValidatorFailure' from
       -- 'JSONSchema.Draft4.Failure'. If your validators have different
       -- failure possibilities you'll need to create your own validator
       -- failure type.
d4Spec schemaMap visited scope =
    Spec $
        [ dimap
            f
            FailureRef
            (D4.refValidator (FE.getReference schemaMap) updateScope
                             valRef visited scope)
        ]

        <> fmap (lmap disableIfRefPresent)
        [ dimap f FailureMultipleOf D4.multipleOfValidator
        , dimap f FailureMaximum D4.maximumValidator
        , dimap f FailureMinimum D4.minimumValidator

        , dimap f FailureMaxLength D4.maxLengthValidator
        , dimap f FailureMinLength D4.minLengthValidator
        , dimap f FailurePattern D4.patternValidator

        , dimap f FailureMaxItems D4.maxItemsValidator
        , dimap f FailureMinItems D4.minItemsValidator
        , dimap f FailureUniqueItems D4.uniqueItemsValidator
        , dimap
            (fromMaybe D4.emptyItems . f)
            (\err -> case err of
                         D4.IRInvalidItems e      -> FailureItems e
                         D4.IRInvalidAdditional e -> FailureAdditionalItems e)
            (D4.itemsRelatedValidator descend)
        , lmap f D4.definitionsEmbedded

        , dimap f FailureMaxProperties D4.maxPropertiesValidator
        , dimap f FailureMinProperties D4.minPropertiesValidator
        , dimap f FailureRequired D4.requiredValidator
        , dimap f FailureDependencies (D4.dependenciesValidator descend)
        , dimap
            (fromMaybe D4.emptyProperties . f)
            FailurePropertiesRelated
            (D4.propertiesRelatedValidator descend)

        , dimap f FailureEnum D4.enumValidator
        , dimap f FailureType D4.typeValidator
        , dimap f FailureAllOf (D4.allOfValidator lateral)
        , dimap f FailureAnyOf (D4.anyOfValidator lateral)
        , dimap f FailureOneOf (D4.oneOfValidator lateral)
        , dimap f FailureNot (D4.notValidator lateral)
        ]
  where
    f :: FromJSON a => Schema -> Maybe a
    f (Schema a) =
        case AE.fromJSON (Object a) of
            AE.Error _   -> Nothing
            AE.Success b -> Just b

    disableIfRefPresent :: Schema -> Schema
    disableIfRefPresent schema =
        case FE._fiRef draft4FetchInfo schema of
            Nothing -> schema
            Just _  -> Schema mempty

    updateScope :: BaseURI -> Schema -> BaseURI
    updateScope uri schema =
        updateResolutionScope uri (FE._fiId draft4FetchInfo schema)

    valRef
        :: D4.VisitedSchemas
        -> Scope Schema
        -> Schema
        -> Value
        -> [ValidatorFailure]
    valRef vis sc = JT.validate (d4Spec schemaMap vis sc)

    descend :: Schema -> Value -> [ValidatorFailure]
    descend = validateSubschema schemaMap mempty scope

    lateral :: Schema -> Value -> [ValidatorFailure]
    lateral = validateSubschema schemaMap visited scope
