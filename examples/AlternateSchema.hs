-- | An implementation of JSON Schema Draft 4 based on 'HashMap Text Value'
-- instead of a custom record type like 'Data.JsonSchema.Draft4'.
--
-- If you're writing code for a new schema specification you probably want
-- to copy this module instead of the 'Data.JsonSchema.Draft4'. While it's
-- less convenient to write schemas in Haskell without a record type, you
-- can get the implementation finished with far fewer lines of code.
--
-- Note that this module imports the the failure sum type and failure related
-- helper functions from the library. If you're implementing a custom schema
-- with different error types from JSON Schema Draft 4 you'll have to make
-- your own.

module AlternateSchema where

import           Data.Aeson                     (FromJSON(..), Value(..),
                                                 decode)
import qualified Data.Aeson                     as AE
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.HashMap.Strict            as HM
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid
import           Data.Profunctor                (Profunctor (..))
import           Data.Text                      (Text)

import           Data.JsonSchema.Draft4         (schemaForSchemasBytes)
import           Data.JsonSchema.Draft4.Failure
import           Data.JsonSchema.Fetch          (ReferencedSchemas(..),
                                                 SchemaWithURI(..))
import qualified Data.JsonSchema.Fetch          as FE
import           Data.JsonSchema.Types          (Schema(..), Spec(..))
import qualified Data.JsonSchema.Types          as JT
import qualified Data.Validator.Draft4          as D4
import qualified Data.Validator.Draft4.Any      as AN
import           Data.Validator.Reference       (updateResolutionScope)

--------------------------------------------------
-- * Basic fetching tools
--------------------------------------------------

referencesViaHTTP
    :: SchemaWithURI Schema
    -> IO (Either FE.HTTPFailure (FE.URISchemaMap Schema))
referencesViaHTTP = FE.referencesViaHTTP' draft4FetchInfo

draft4FetchInfo :: FE.FetchInfo Schema
draft4FetchInfo = FE.FetchInfo embedded (get "id") (get "$ref")
  where
    get :: Text -> Schema -> Maybe Text
    get k (Schema s) = case HM.lookup k s of
                           Just (String t) -> Just t
                           _ -> Nothing

embedded :: Schema -> ([Schema], [Schema])
embedded s = JT.embedded (d4Spec (ReferencedSchemas s mempty) mempty Nothing) s

--------------------------------------------------
-- * Main API
--------------------------------------------------

validate
    :: ReferencedSchemas Schema
    -> Maybe Text
    -> Schema
    -> Value
    -> [Failure]
validate rs = continueValidating rs (AN.VisitedSchemas [(Nothing, Nothing)])

-- A schema for schemas themselves, using @src/draft4.json@ which is loaded
-- at compile time.
schemaForSchemas :: Schema
schemaForSchemas =
      fromMaybe (error "Schema decode failed (this should never happen)")
    . decode
    . LBS.fromStrict
    $ schemaForSchemasBytes

checkSchema :: Schema -> [Failure]
checkSchema = validate referenced Nothing schemaForSchemas . Object . _unSchema
  where
    referenced :: ReferencedSchemas Schema
    referenced = ReferencedSchemas
                     schemaForSchemas
                     (HM.singleton "http://json-schema.org/draft-04/schema"
                         schemaForSchemas)

--------------------------------------------------
-- * Spec
--------------------------------------------------

continueValidating
    :: ReferencedSchemas Schema
    -> AN.VisitedSchemas
    -> Maybe Text
    -> Schema
    -> Value
    -> [Failure]
continueValidating referenced visited mURI sc =
    JT.validate (d4Spec referenced visited newScope) sc
  where
    schemaId :: Maybe Text
    schemaId = case HM.lookup "id" (_unSchema sc) of
                   Just (String t) -> Just t
                   _               -> Nothing

    newScope :: Maybe Text
    newScope = updateResolutionScope mURI schemaId

d4Spec
    :: ReferencedSchemas Schema
    -> AN.VisitedSchemas
    -> Maybe Text
    -> Spec Schema ValidatorChain
d4Spec referenced visited scope =
    Spec
        [ dimap f (const MultipleOf) D4.multipleOf
        , dimap f maxE D4.maximumVal
        , dimap f minE D4.minimumVal

        , dimap f (const MaxLength) D4.maxLength
        , dimap f (const MinLength) D4.minLength
        , dimap f (const PatternValidator) D4.patternVal

        , dimap f (const MaxItems) D4.maxItems
        , dimap f (const MinItems) D4.minItems
        , dimap f (const UniqueItems) D4.uniqueItems
        , dimap f itemsE (D4.items descend)

        , dimap f (const MaxProperties) D4.maxProperties
        , dimap f (const MinProperties) D4.minProperties
        , dimap f (const Required) D4.required
        , dimap f depsE (D4.dependencies descend)
        , dimap f propE (D4.properties descend)
        , dimap f patPropE (D4.patternProperties descend)
        , dimap f addPropE (D4.additionalProperties descend)

        , dimap f refE (D4.ref visited scope (FE.getReference referenced) refVal)
        , dimap f (const Enum) D4.enumVal
        , dimap f (const TypeValidator) D4.typeVal
        , dimap f AllOf (D4.allOf lateral)
        , dimap f (const AnyOf) (D4.anyOf lateral)
        , dimap f (const OneOf) (D4.oneOf lateral)
        , dimap f (const NotValidator) (D4.notVal lateral)
        ]
  where
    f :: FromJSON a => Schema -> Maybe a
    f (Schema a) = case AE.fromJSON (Object a) of
                       AE.Error _   -> Nothing
                       AE.Success b -> Just b

    -- 'Maybe Text' is the URI the refernced schema is fetch from,
    -- this probably needs a 'newtype' wrapper.
    refVal :: AN.VisitedSchemas -> Maybe Text -> Schema -> Value -> [Failure]
    refVal = continueValidating referenced

    descend :: Schema -> Value -> [Failure]
    descend = continueValidating referenced mempty scope

    lateral :: Schema -> Value -> [Failure]
    lateral = continueValidating referenced visited scope
