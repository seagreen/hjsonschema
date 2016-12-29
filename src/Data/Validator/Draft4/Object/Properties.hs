
module Data.Validator.Draft4.Object.Properties where

import           Import

import qualified Data.Hashable         as HA
import qualified Data.HashMap.Strict   as HM
import qualified Data.List.NonEmpty    as NE
import           Data.Text.Encoding    (encodeUtf8)
import qualified JSONPointer           as JP
import qualified Text.Regex.PCRE.Heavy as RE

data PropertiesRelated schema = PropertiesRelated
    { _propProperties :: Maybe (HashMap Text schema)
    , _propPattern    :: Maybe (HashMap Text schema)
    , _propAdditional :: Maybe (AdditionalProperties schema)
    } deriving (Eq, Show)

instance FromJSON schema => FromJSON (PropertiesRelated schema) where
    parseJSON = withObject "PropertiesRelated" $ \o -> PropertiesRelated
        <$> o .:! "properties"
        <*> o .:! "patternProperties"
        <*> o .:! "additionalProperties"

emptyProperties :: PropertiesRelated schema
emptyProperties = PropertiesRelated
    { _propProperties = Nothing
    , _propPattern    = Nothing
    , _propAdditional = Nothing
    }

data AdditionalProperties schema
    = AdditionalPropertiesBool Bool
    | AdditionalPropertiesObject schema
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (AdditionalProperties schema) where
    parseJSON v = fmap AdditionalPropertiesBool (parseJSON v)
              <|> fmap AdditionalPropertiesObject (parseJSON v)

instance ToJSON schema => ToJSON (AdditionalProperties schema) where
    toJSON (AdditionalPropertiesBool b)    = toJSON b
    toJSON (AdditionalPropertiesObject hm) = toJSON hm

instance Arbitrary schema => Arbitrary (AdditionalProperties schema) where
    arbitrary = oneof [ AdditionalPropertiesBool <$> arbitrary
                      , AdditionalPropertiesObject <$> arbitrary
                      ]

-- | A glorified @type@ alias.
newtype Regex
    = Regex { _unRegex :: Text }
    deriving (Eq, Show, Generic)

instance HA.Hashable Regex

-- NOTE: We'd like to enforce that at least one error exists here.
data PropertiesRelatedInvalid err = PropertiesRelatedInvalid
    { _prInvalidProperties :: HashMap Text [err]
    , _prInvalidPattern    :: HashMap (Regex, JP.Key) [err]
    , _prInvalidAdditional :: Maybe (APInvalid err)
    } deriving (Eq, Show)

data APInvalid err
    = APBoolInvalid   (HashMap Text Value)
    | APObjectInvalid (HashMap Text (NonEmpty err))
    deriving (Eq, Show)

-- | First @"properties"@ and @"patternProperties"@ are run simultaneously
-- on the data, then @"additionalProperties"@ is run on the remainder.
propertiesRelatedVal
    :: forall err schema.
       (schema -> Value -> [err])
    -> PropertiesRelated schema
    -> HashMap Text Value
    -> Maybe (PropertiesRelatedInvalid err)
propertiesRelatedVal f props x
    |  all null (HM.elems propFailures)
    && all null (HM.elems patFailures)
    && isNothing addFailures = Nothing
    | otherwise =
        Just PropertiesRelatedInvalid
            { _prInvalidProperties = propFailures
            , _prInvalidPattern    = patFailures
            , _prInvalidAdditional = addFailures
            }
  where
    propertiesHm :: HashMap Text schema
    propertiesHm = fromMaybe mempty (_propProperties props)

    patHm :: HashMap Text schema
    patHm = fromMaybe mempty (_propPattern props)

    propAndUnmatched :: (HashMap Text [err], Remaining)
    propAndUnmatched = ( HM.intersectionWith f propertiesHm x
                       , Remaining (HM.difference x propertiesHm)
                       )

    (propFailures, propRemaining) = propAndUnmatched

    patAndUnmatched :: (HashMap (Regex, JP.Key) [err], Remaining)
    patAndUnmatched = patternAndUnmatched f patHm x

    (patFailures, patRemaining) = patAndUnmatched

    finalRemaining :: Remaining
    finalRemaining = Remaining (HM.intersection (_unRemaining patRemaining)
                                                (_unRemaining propRemaining))

    addFailures :: Maybe (APInvalid err)
    addFailures = (\addProp -> additionalProperties f addProp finalRemaining)
              =<< _propAdditional props

-- | Internal.
newtype Remaining
    = Remaining { _unRemaining :: HashMap Text Value }

-- | Internal.
patternAndUnmatched
    :: forall err schema.
       (schema -> Value -> [err])
    -> HashMap Text schema
    -> HashMap Text Value
    -> (HashMap (Regex, JP.Key) [err], Remaining)
patternAndUnmatched f patPropertiesHm x =
    (HM.foldlWithKey' runMatches mempty perhapsMatches, remaining)
  where
    -- @[(Regex, schema)]@ will have one item per match.
    perhapsMatches :: HashMap Text ([(Regex, schema)], Value)
    perhapsMatches =
        HM.foldlWithKey' (matchingSchemas patPropertiesHm) mempty x
      where
        matchingSchemas
            :: HashMap Text schema
            -> HashMap Text ([(Regex, schema)], Value)
            -> Text
            -> Value
            -> HashMap Text ([(Regex, schema)], Value)
        matchingSchemas subSchemas acc k v =
            HM.insert k
                      (HM.foldlWithKey' (checkKey k) mempty subSchemas, v)
                      acc

        checkKey
            :: Text
            -> [(Regex, schema)]
            -> Text
            -> schema
            -> [(Regex, schema)]
        checkKey k acc r subSchema =
            case RE.compileM (encodeUtf8 r) mempty of
                Left _   -> acc
                Right re -> if k RE.=~ re
                                then (Regex r, subSchema) : acc
                                else acc

    runMatches
        :: HashMap (Regex, JP.Key) [err]
        -> Text
        -> ([(Regex, schema)], Value)
        -> HashMap (Regex, JP.Key) [err]
    runMatches acc k (matches,v) =
        foldr runMatch acc matches
      where
        runMatch
            :: (Regex, schema)
            -> HashMap (Regex, JP.Key) [err]
            -> HashMap (Regex, JP.Key) [err]
        runMatch (r,schema) = HM.insert (r, JP.Key k) (f schema v)

    remaining :: Remaining
    remaining = Remaining . fmap snd . HM.filter (null . fst) $ perhapsMatches

-- Internal.
additionalProperties
    :: forall err schema.
       (schema -> Value -> [err])
    -> AdditionalProperties schema
    -> Remaining
    -> Maybe (APInvalid err)
additionalProperties f a x =
    case a of
        AdditionalPropertiesBool b ->
            APBoolInvalid <$> additionalPropertiesBool b x
        AdditionalPropertiesObject b ->
            APObjectInvalid <$> additionalPropertiesObject f b x

-- | Internal.
additionalPropertiesBool
    :: Bool
    -> Remaining
    -> Maybe (HashMap Text Value)
additionalPropertiesBool True _ = Nothing
additionalPropertiesBool False (Remaining x)
    | HM.size x > 0 = Just x
    | otherwise     = Nothing

-- | Internal.
additionalPropertiesObject
    :: forall err schema.
       (schema -> Value -> [err])
    -> schema
    -> Remaining
    -> Maybe (HashMap Text (NonEmpty err))
additionalPropertiesObject f schema (Remaining x) =
    let errs = HM.mapMaybe (NE.nonEmpty . f schema) x
    in if HM.null errs
        then Nothing
        else Just errs
