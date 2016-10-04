-- | Turn the validation functions into actual 'Validator's.
--
-- From this point on they know how to parse themselves from JSON
-- and also know how to extract subschemas embedded within themselves.

module Data.Validator.Draft4 where

import           Prelude
import           Import

import           Data.Aeson.Types             (Parser)
import qualified Data.HashMap.Strict          as HM
import           Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty           as NE
import           Data.Maybe                   (catMaybes, isNothing,
                                               maybe, maybeToList)
import           Data.Scientific              (Scientific)
import           Data.Text                    (Text)

import qualified Data.Validator.Draft4.Any    as AN
import qualified Data.Validator.Draft4.Array  as AR
import qualified Data.Validator.Draft4.Number as NU
import qualified Data.Validator.Draft4.Object as OB
import qualified Data.Validator.Draft4.String as ST
import           Data.Validator.Failure       (Fail(..))
import           Data.Validator.Types         (Validator(..))
import           Data.Validator.Utils         (fromJSONEither)

-- | For internal use.
run :: FromJSON b => (a -> b -> [c]) -> Maybe a -> Value -> [c]
run _ Nothing _  = mempty
run f (Just a) b =
    case fromJSONEither b of
        Left _  -> mempty
        Right c -> f a c

-- | For internal use.
noEmbedded :: a -> ([b], [b])
noEmbedded = const (mempty, mempty)

--------------------------------------------------
-- * Numbers
--------------------------------------------------

newtype MultipleOf
    = MultipleOf { _unMultipleOf :: Scientific }
    deriving (Eq, Show)

instance FromJSON MultipleOf where
    parseJSON = withObject "MultipleOf" $ \o ->
        MultipleOf <$> o .: "multipleOf"

multipleOf :: Validator a (Maybe MultipleOf) ()
multipleOf =
    Validator
        noEmbedded
        (run (fmap maybeToList . NU.multipleOf . _unMultipleOf))

data MaximumContext
    = MaximumContext Bool Scientific
    deriving (Eq, Show)

instance FromJSON MaximumContext where
    parseJSON = withObject "MaximumContext" $ \o -> MaximumContext
        <$> o .:! "exclusiveMaximum" .!= False
        <*> o .: "maximum"

maximumVal :: Validator a (Maybe MaximumContext) NU.MaximumInvalid
maximumVal =
    Validator
        noEmbedded
        (run (\(MaximumContext a b) -> maybeToList . NU.maximumVal a b))

data MinimumContext
    = MinimumContext Bool Scientific
    deriving (Eq, Show)

instance FromJSON MinimumContext where
    parseJSON = withObject "MinimumContext" $ \o -> MinimumContext
        <$> o .:! "exclusiveMinimum" .!= False
        <*> o .: "minimum"

minimumVal :: Validator a (Maybe MinimumContext) NU.MinimumInvalid
minimumVal =
    Validator
        noEmbedded
        (run (\(MinimumContext a b) -> maybeToList . NU.minimumVal a b))

--------------------------------------------------
-- * Strings
--------------------------------------------------

newtype MaxLength
    = MaxLength { _unMaxLength :: Int }
    deriving (Eq, Show)

instance FromJSON MaxLength where
    parseJSON = withObject "MaxLength" $ \o ->
        MaxLength <$> o .: "maxLength"

maxLength :: Validator a (Maybe MaxLength) ()
maxLength =
    Validator
        noEmbedded
        (run (fmap maybeToList . ST.maxLength . _unMaxLength))

newtype MinLength
    = MinLength { _unMinLength :: Int }
    deriving (Eq, Show)

instance FromJSON MinLength where
    parseJSON = withObject "MinLength" $ \o ->
        MinLength <$> o .: "minLength"

minLength :: Validator a (Maybe MinLength) ()
minLength =
    Validator
        noEmbedded
        (run (fmap maybeToList . ST.minLength . _unMinLength))

newtype PatternVal
    = PatternVal { _unPatternVal :: Text }
    deriving (Eq, Show)

instance FromJSON PatternVal where
    parseJSON = withObject "PatternVal" $ \o ->
        PatternVal <$> o .: "pattern"

patternVal :: Validator a (Maybe PatternVal) ()
patternVal =
    Validator
        noEmbedded
        (run (fmap maybeToList . ST.patternVal . _unPatternVal))

--------------------------------------------------
-- * Arrays
--------------------------------------------------

newtype MaxItems
    = MaxItems { _unMaxItems :: Int }
    deriving (Eq, Show)

instance FromJSON MaxItems where
    parseJSON = withObject "MaxItems" $ \o ->
        MaxItems <$> o .: "maxItems"

maxItems :: Validator a (Maybe MaxItems) ()
maxItems =
    Validator
        noEmbedded
        (run (fmap maybeToList . AR.maxItems . _unMaxItems))

newtype MinItems
    = MinItems { _unMinItems :: Int }
    deriving (Eq, Show)

instance FromJSON MinItems where
    parseJSON = withObject "MinItems" $ \o ->
        MinItems <$> o .: "minItems"

minItems :: Validator a (Maybe MinItems) ()
minItems =
    Validator
        noEmbedded
        (run (fmap maybeToList . AR.minItems . _unMinItems))

newtype UniqueItems
    = UniqueItems { _unUniqueItems :: Bool }
    deriving (Eq, Show)

instance FromJSON UniqueItems where
    parseJSON = withObject "UniqueItems" $ \o ->
        UniqueItems <$> o .: "uniqueItems"

uniqueItems :: Validator a (Maybe UniqueItems) ()
uniqueItems =
    Validator
        noEmbedded
        (run (fmap maybeToList . AR.uniqueItems . _unUniqueItems))

data ItemsContext schema =
    ItemsContext
        (Maybe (AR.AdditionalItems schema))
        (AR.Items schema)
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (ItemsContext schema) where
    parseJSON = withObject "ItemsContext" $ \o -> ItemsContext
        <$> o .:! "additionalItems"
        <*> o .: "items"

items
    :: (schema -> Value -> [Fail err])
    -> Validator schema (Maybe (ItemsContext schema)) (AR.ItemsInvalid err)
items f =
    Validator
        (\a -> case a of
                   Nothing -> mempty
                   Just (ItemsContext _ b) ->
                       case b of
                           AR.ItemsObject c -> (mempty, pure c)
                           AR.ItemsArray cs -> (mempty, cs))
        (run (\(ItemsContext a b) -> AR.items f a b))

newtype AdditionalItemsContext schema
    = AdditionalItemsContext
        { _unAdditionalItemsContext :: AR.AdditionalItems schema }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (AdditionalItemsContext schema) where
    parseJSON = withObject "AdditionalItemsContext" $ \o ->
        AdditionalItemsContext <$> o .: "additionalItems"

-- | Since 'items' will always take care of validating 'additionalItems'
-- as well, the actual validation side of 'additionalItemsEmbedded' is
-- disabled.
additionalItemsEmbedded
    :: Validator
           schema
           (Maybe (AdditionalItemsContext schema))
           err
additionalItemsEmbedded=
    Validator
        (\a -> case a of
                   Just (AdditionalItemsContext (AR.AdditionalObject b)) ->
                       (mempty, pure b)
                   _ -> (mempty, mempty))
        (const (const mempty))

newtype Definitions schema
    = Definitions { _unDefinitions :: HashMap Text schema }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (Definitions schema) where
    parseJSON = withObject "Definitions" $ \o ->
        Definitions <$> o .: "definitions"

-- | Placing this here since it's similar to @"additionalItems"@.
-- in that its validator doesn't run.
--
-- TODO: Add tests to the language agnostic test suite for both
-- @"additionalItems"@ and this.
definitionsEmbedded
    :: Validator
           schema
           (Maybe (Definitions schema))
           err
definitionsEmbedded =
    Validator
        (\a -> case a of
                 Just (Definitions b) -> (mempty, HM.elems b)
                 Nothing              -> (mempty, mempty))
        (const (const mempty))

--------------------------------------------------
-- * Objects
--------------------------------------------------

newtype MaxProperties
    = MaxProperties { _unMaxProperties :: Int }
    deriving (Eq, Show)

instance FromJSON MaxProperties where
    parseJSON = withObject "MaxProperties" $ \o ->
        MaxProperties <$> o .: "maxProperties"

maxProperties :: Validator a (Maybe MaxProperties) ()
maxProperties =
    Validator
        noEmbedded
        (run (fmap maybeToList . OB.maxProperties . _unMaxProperties))

newtype MinProperties
    = MinProperties { _unMinProperties :: Int }
    deriving (Eq, Show)

instance FromJSON MinProperties where
    parseJSON = withObject "MinProperties" $ \o ->
        MinProperties <$> o .: "minProperties"

minProperties :: Validator a (Maybe MinProperties) ()
minProperties =
    Validator
        noEmbedded
        (run (fmap maybeToList . OB.minProperties . _unMinProperties))

newtype RequiredContext
    = RequiredContext { _unRequiredContext :: OB.Required }
    deriving (Eq, Show)

instance FromJSON RequiredContext where
    parseJSON = withObject "RequiredContext" $ \o ->
        RequiredContext <$> o .: "required"

required :: Validator a (Maybe RequiredContext) ()
required =
    Validator
        noEmbedded
        (run (fmap maybeToList . OB.required . _unRequiredContext))

newtype DependenciesContext schema
    = DependenciesContext
           { _unDependenciesContext :: HashMap Text (OB.Dependency schema) }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (DependenciesContext schema) where
    parseJSON = withObject "DependenciesContext" $ \o ->
        DependenciesContext <$> o .: "dependencies"

dependencies
    :: (schema -> Value -> [Fail err])
    -> Validator
           schema
           (Maybe (DependenciesContext schema))
           (OB.DependencyInvalid err)
dependencies f =
    Validator
        (maybe mempty ( (\a -> (mempty, a))
                      . catMaybes . fmap checkDependency
                      . HM.elems . _unDependenciesContext
                      ))
        (run (OB.dependencies f . _unDependenciesContext))
  where
    checkDependency :: OB.Dependency schema -> Maybe schema
    checkDependency (OB.PropertyDependency _) = Nothing
    checkDependency (OB.SchemaDependency s)   = Just s

data PropertiesContext schema
    = PropertiesContext
          (Maybe (HashMap Text schema))
          (Maybe (OB.AdditionalProperties schema))
          (HashMap Text schema)
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (PropertiesContext schema) where
    parseJSON = withObject "PropertiesContext" $ \o -> PropertiesContext
        <$> o .:! "patternProperties"
        <*> o .:! "additionalProperties"
        <*> o .: "properties"

properties
    :: (schema -> Value -> [Fail err])
    -> Validator
           schema
           (Maybe (PropertiesContext schema))
           (OB.PropertiesInvalid err)
properties f =
    Validator
        (\a -> case a of
                   Just (PropertiesContext _ _ b) -> (mempty, HM.elems b)
                   Nothing                        -> (mempty, mempty))
        (run (\(PropertiesContext a b c) -> OB.properties f a b c))

-- | The first argument is whether the validator should be run.
-- If @"properties"@ exists it will be parsed to 'False'.
data PatternPropertiesContext schema
    = PatternPropertiesContext
          Bool
          (Maybe (OB.AdditionalProperties schema))
          (HashMap Text schema)
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (PatternPropertiesContext schema) where
    parseJSON = withObject "PatternPropertiesContext" $ \o ->
        PatternPropertiesContext
            <$> shouldRun o
            <*> o .:! "additionalProperties"
            <*> o .: "patternProperties"
      where
        shouldRun :: HashMap Text Value -> Parser Bool
        shouldRun o = do
            a <- o .:! "properties"
            pure $ isNothing (a :: Maybe (HashMap Text schema))

patternProperties
    :: (schema -> Value -> [Fail err])
    -> Validator
           schema
           (Maybe (PatternPropertiesContext schema))
           (OB.PatternPropertiesInvalid err)
patternProperties f =
    Validator
        (\a -> case a of
                   Just (PatternPropertiesContext _ _ b) -> (mempty, HM.elems b)
                   Nothing                               -> (mempty, mempty))
        (run (\(PatternPropertiesContext a b c) -> OB.patternProperties f a b c))

-- | The first argument is whether the validator should be run.
-- If @"properties"@ or @"patternProperties"@ exist it will be parsed
-- to 'False'.
data AdditionalPropertiesContext schema
    = AdditionalPropertiesContext
          Bool
          (OB.AdditionalProperties schema)
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (AdditionalPropertiesContext schema) where
    parseJSON = withObject "AdditionalPropertiesContext" $ \o ->
        AdditionalPropertiesContext
            <$> shouldRun o
            <*> o .: "additionalProperties"
      where
        shouldRun :: HashMap Text Value -> Parser Bool
        shouldRun o = do
            a <- o .:! "properties"
            b <- o .:! "patternProperties"
            pure $ isNothing (a :: Maybe (HashMap Text schema))
                && isNothing (b :: Maybe (HashMap Text schema))

additionalProperties
    :: (schema -> Value -> [Fail err])
    -> Validator
           schema
           (Maybe (AdditionalPropertiesContext schema))
           (OB.AdditionalPropertiesInvalid err)
additionalProperties f =
    Validator
        (\a -> case a of
                   Nothing -> mempty
                   Just (AdditionalPropertiesContext _ b) ->
                       case b of
                           OB.AdditionalPropertiesBool _   -> (mempty, mempty)
                           OB.AdditionalPropertiesObject c -> (mempty, pure c))
        (run (\(AdditionalPropertiesContext a b) -> OB.additionalProperties f a b))

--------------------------------------------------
-- * Any
--------------------------------------------------

newtype Ref
    = Ref { _unRef :: Text }
    deriving (Eq, Show)

instance FromJSON Ref where
    parseJSON = withObject "Ref" $ \o ->
        Ref <$> o .: "$ref"

ref
    :: (FromJSON schema, ToJSON schema)
    => AN.VisitedSchemas
    -> Maybe Text
    -> (Maybe Text -> Maybe schema)
    -> (AN.VisitedSchemas -> Maybe Text -> schema -> Value -> [Fail err])
    -> Validator a (Maybe Ref) (AN.RefInvalid err)
ref visited scope getRef f =
    Validator
        noEmbedded
        (run (AN.ref visited scope getRef f . _unRef))

newtype EnumContext
    = EnumContext { _unEnumContext :: AN.EnumVal }
    deriving (Eq, Show)

instance FromJSON EnumContext where
    parseJSON = withObject "EnumContext" $ \o ->
        EnumContext <$> o .: "enum"

enumVal :: Validator a (Maybe EnumContext) ()
enumVal =
    Validator
        noEmbedded
        (run (fmap maybeToList . AN.enumVal . _unEnumContext))

newtype TypeContext
    = TypeContext { _unTypeContext :: AN.TypeVal }
    deriving (Eq, Show)

instance FromJSON TypeContext where
    parseJSON = withObject "TypeContext" $ \o ->
        TypeContext <$> o .: "type"

typeVal :: Validator a (Maybe TypeContext) ()
typeVal =
    Validator
        noEmbedded
        (run (fmap maybeToList . AN.typeVal . _unTypeContext))

newtype AllOf schema
    = AllOf { _unAllOf :: NonEmpty schema }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (AllOf schema) where
    parseJSON = withObject "AllOf" $ \o ->
        AllOf <$> o .: "allOf"

allOf
    :: (schema -> Value -> [Fail err])
    -> Validator schema (Maybe (AllOf schema)) err
allOf f =
    Validator
        (\a -> case a of
                   Just (AllOf b) -> (NE.toList b, mempty)
                   Nothing        -> (mempty, mempty))
        (run (AN.allOf f . _unAllOf))

newtype AnyOf schema
    = AnyOf { _unAnyOf :: NonEmpty schema }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (AnyOf schema) where
    parseJSON = withObject "AnyOf" $ \o ->
        AnyOf <$> o .: "anyOf"

anyOf
    :: (schema -> Value -> [Fail err])
    -> Validator schema (Maybe (AnyOf schema)) err
anyOf f =
    Validator
        (\a -> case a of
                 Just (AnyOf b) -> (NE.toList b, mempty)
                 Nothing        -> (mempty, mempty))
        (run (AN.anyOf f . _unAnyOf))

newtype OneOf schema
    = OneOf { _unOneOf :: NonEmpty schema }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (OneOf schema) where
    parseJSON = withObject "OneOf" $ \o ->
        OneOf <$> o .: "oneOf"

oneOf
    :: ToJSON schema
    => (schema -> Value -> [Fail err])
    -> Validator schema (Maybe (OneOf schema)) (AN.OneOfInvalid err)
oneOf f =
    Validator
        (\a -> case a of
                   Just (OneOf b) -> (NE.toList b, mempty)
                   Nothing        -> (mempty, mempty))
        (run (AN.oneOf f . _unOneOf))

newtype NotVal schema
    = NotVal { _unNotVal :: schema }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (NotVal schema) where
    parseJSON = withObject "NotVal" $ \o ->
        NotVal <$> o .: "not"

notVal
    :: ToJSON schema
    => (schema -> Value -> [Fail err])
    -> Validator schema (Maybe (NotVal schema)) ()
notVal f =
    Validator
        (\a -> case a of
                   Just (NotVal b) -> (pure b, mempty)
                   Nothing         -> (mempty, mempty))
        (run (fmap maybeToList . AN.notVal f . _unNotVal))
