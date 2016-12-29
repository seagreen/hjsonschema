-- | Turn the validation functions into actual 'Validator's.

module Data.Validator.Draft4
    ( module Data.Validator.Draft4
    , module Export
    ) where

import           Import

import qualified Data.HashMap.Strict          as HM
import qualified Data.List.NonEmpty           as NE
import           Data.Maybe                   (catMaybes, maybe, maybeToList)
import           Data.Text                    (Text)

import           Data.Validator.Draft4.Any    as Export
import           Data.Validator.Draft4.Array  as Export
import           Data.Validator.Draft4.Number as Export
import           Data.Validator.Draft4.Object as Export
import           Data.Validator.Draft4.String as Export
import           Data.Validator.Types         (Validator(..))
import           Data.Validator.Utils         (fromJSONEither)

-- | For internal use.
--
-- Take a validation function, a possibly existing validator, and some data.
-- If the validator is exists and can validate the type of data we have,
-- attempt to do so and return any failures.
run :: FromJSON b => (a -> b -> Maybe c) -> Maybe a -> Value -> [c]
run _ Nothing _  = mempty
run f (Just a) b =
    case fromJSONEither b of
        Left _  -> mempty
        Right c -> maybeToList (f a c)

-- | For internal use.
noEmbedded :: a -> ([b], [b])
noEmbedded = const (mempty, mempty)

--------------------------------------------------
-- * Numbers
--------------------------------------------------

multipleOfValidator :: Validator a (Maybe MultipleOf) MultipleOfInvalid
multipleOfValidator = Validator noEmbedded (run multipleOfVal)

maximumValidator :: Validator a (Maybe Maximum) MaximumInvalid
maximumValidator = Validator noEmbedded (run maximumVal)

minimumValidator :: Validator a (Maybe Minimum) MinimumInvalid
minimumValidator = Validator noEmbedded (run minimumVal)

--------------------------------------------------
-- * Strings
--------------------------------------------------

maxLengthValidator :: Validator a (Maybe MaxLength) MaxLengthInvalid
maxLengthValidator = Validator noEmbedded (run maxLengthVal)

minLengthValidator :: Validator a (Maybe MinLength) MinLengthInvalid
minLengthValidator = Validator noEmbedded (run minLengthVal)

patternValidator :: Validator a (Maybe PatternValidator) PatternInvalid
patternValidator = Validator noEmbedded (run patternVal)

--------------------------------------------------
-- * Arrays
--------------------------------------------------

maxItemsValidator :: Validator a (Maybe MaxItems) MaxItemsInvalid
maxItemsValidator = Validator noEmbedded (run maxItemsVal)

minItemsValidator :: Validator a (Maybe MinItems) MinItemsInvalid
minItemsValidator = Validator noEmbedded (run minItemsVal)

uniqueItemsValidator :: Validator a (Maybe UniqueItems) UniqueItemsInvalid
uniqueItemsValidator = Validator noEmbedded (run uniqueItemsVal)

itemsRelatedValidator
    :: (schema -> Value -> [err])
    -> Validator schema (ItemsRelated schema) (ItemsRelatedInvalid err)
itemsRelatedValidator f =
    Validator
        (\a -> ( mempty
               ,  case _irItems a of
                      Just (ItemsObject b) -> pure b
                      Just (ItemsArray cs) -> cs
                      Nothing              -> mempty
               <> case _irAdditional a of
                      Just (AdditionalObject b) -> pure b
                      _                         -> mempty
               ))
        (\a b -> case fromJSONEither b of
                     Left _  -> mempty
                     Right c -> itemsRelatedVal f a c)

--------------------------------------------------
-- * Objects
--------------------------------------------------

maxPropertiesValidator
    :: Validator
           a
           (Maybe MaxProperties)
           MaxPropertiesInvalid
maxPropertiesValidator = Validator noEmbedded (run maxPropertiesVal)

minPropertiesValidator
    :: Validator
           a
           (Maybe MinProperties)
           MinPropertiesInvalid
minPropertiesValidator = Validator noEmbedded (run minPropertiesVal)

requiredValidator :: Validator a (Maybe Required) ()
requiredValidator = Validator noEmbedded (run requiredVal)

dependenciesValidator
    :: (schema -> Value -> [err])
    -> Validator
           schema
           (Maybe (DependenciesValidator schema))
           (DependenciesInvalid err)
dependenciesValidator f =
    Validator
        (maybe mempty ( (\a -> (mempty, a))
                      . catMaybes . fmap checkDependency
                      . HM.elems . _unDependenciesValidator
                      ))
        (run (dependenciesVal f))
  where
    checkDependency :: Dependency schema -> Maybe schema
    checkDependency (PropertyDependency _) = Nothing
    checkDependency (SchemaDependency s)   = Just s

propertiesRelatedValidator
    :: (schema -> Value -> [err])
    -> Validator
           schema
           (PropertiesRelated schema)
           (PropertiesRelatedInvalid err)
propertiesRelatedValidator f =
    Validator
        (\a -> ( mempty
               ,  HM.elems (fromMaybe mempty (_propProperties a))
               <> HM.elems (fromMaybe mempty (_propPattern a))
               <> case _propAdditional a of
                      Just (AdditionalPropertiesObject b) -> [b]
                      _ -> mempty
               ))
        (\a b -> case fromJSONEither b of
                     Left _  -> mempty
                     Right c -> maybeToList (propertiesRelatedVal f a c))

newtype Definitions schema
    = Definitions { _unDefinitions :: HashMap Text schema }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (Definitions schema) where
    parseJSON = withObject "Definitions" $ \o ->
        Definitions <$> o .: "definitions"

-- TODO: Add tests to the language agnostic test suite to
-- make sure these schemas are embedded correctly
-- (and do so for @"additionalItems"@ as well).
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
-- * Any
--------------------------------------------------

refValidator
    :: (FromJSON schema, ToJSON schema)
    => VisitedSchemas
    -> Maybe Text
    -> (Maybe Text -> Maybe schema)
    -> (VisitedSchemas -> Maybe Text -> schema -> Value -> [err])
    -> Validator a (Maybe Ref) (RefInvalid err)
refValidator visited scope getRef f =
    Validator
        noEmbedded
        (run (refVal visited scope getRef f))

enumValidator :: Validator a (Maybe EnumValidator) EnumInvalid
enumValidator = Validator noEmbedded (run enumVal)

typeValidator :: Validator a (Maybe TypeContext) TypeValidatorInvalid
typeValidator = Validator noEmbedded (run typeVal)

allOfValidator
    :: (schema -> Value -> [err])
    -> Validator schema (Maybe (AllOf schema)) (AllOfInvalid err)
allOfValidator f =
    Validator
        (\a -> case a of
                   Just (AllOf b) -> (NE.toList b, mempty)
                   Nothing        -> (mempty, mempty))
        (run (allOfVal f))

anyOfValidator
    :: (schema -> Value -> [err])
    -> Validator schema (Maybe (AnyOf schema)) (AnyOfInvalid err)
anyOfValidator f =
    Validator
        (\a -> case a of
                 Just (AnyOf b) -> (NE.toList b, mempty)
                 Nothing        -> (mempty, mempty))
        (run (anyOfVal f))

oneOfValidator
    :: ToJSON schema
    => (schema -> Value -> [err])
    -> Validator schema (Maybe (OneOf schema)) (OneOfInvalid err)
oneOfValidator f =
    Validator
        (\a -> case a of
                   Just (OneOf b) -> (NE.toList b, mempty)
                   Nothing        -> (mempty, mempty))
        (run (oneOfVal f))

notValidator
    :: ToJSON schema
    => (schema -> Value -> [err])
    -> Validator
           schema
           (Maybe (NotValidator schema))
           NotValidatorInvalid
notValidator f =
    Validator
        (\a -> case a of
                   Just (NotValidator b) -> (pure b, mempty)
                   Nothing               -> (mempty, mempty))
        (run (notVal f))
