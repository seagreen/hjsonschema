
module Data.Validator.Draft4.Object.Properties where

import           Import
import           Prelude

import           Control.Monad
import qualified Data.Aeson.Pointer     as AP
import           Data.Functor           (($>))
import qualified Data.HashMap.Strict    as HM
import           Data.Text.Encoding     (encodeUtf8)
import qualified Text.Regex.PCRE.Heavy  as RE

import           Data.Validator.Failure (Fail(..), prependToPath)

-- | For internal use.
newtype Remaining
    = Remaining { _unRemaining :: HashMap Text Value }

--------------------------------------------------
-- * properties
--------------------------------------------------

data PropertiesInvalid err
    = PropertiesInvalid err
    | PropPatternInvalid err
    | PropAdditionalInvalid (AdditionalPropertiesInvalid err)
    deriving (Eq, Show)

-- | In order of what's tried: @"properties"@, @"patternProperties"@,
-- @"additionalProperties"@.
properties
    :: forall err schema.
       (schema -> Value -> [Fail err])
    -> Maybe (HashMap Text schema)
    -> Maybe (AdditionalProperties schema)
    -> HashMap Text schema
    -> HashMap Text Value
    -> [Fail (PropertiesInvalid err)]
properties f mPat mAdd propertiesHm x =
       fmap (fmap PropertiesInvalid) propFailures
    <> fmap (fmap PropPatternInvalid) patternFailures
    <> fmap (fmap PropAdditionalInvalid) additionalFailures
  where
    propertiesAndUnmatched :: ([Fail err], Remaining)
    propertiesAndUnmatched = ( failures
                             , Remaining (HM.difference x propertiesHm)
                             )
      where
        failures :: [Fail err]
        failures = HM.toList (HM.intersectionWith f propertiesHm x)
               >>= (\(k,vs) -> fmap (prependToPath (AP.Token k)) vs)

    (propFailures, remaining1) = propertiesAndUnmatched

    mPatProp :: Maybe (HashMap Text Value -> ([Fail err], Remaining))
    mPatProp = patternAndUnmatched f <$> mPat

    patternFailures :: [Fail err]
    patternFailures = case mPatProp of
                          Nothing  -> mempty
                          Just val -> fst (val x)

    remaining2 :: Remaining
    remaining2 = case mPatProp of
                     Nothing  -> remaining1
                     Just val -> snd . val . _unRemaining $ remaining1

    additionalFailures :: [Fail (AdditionalPropertiesInvalid err)]
    additionalFailures =
        case mAdd of
            Nothing -> mempty
            Just a  -> additionalProperties f True a (_unRemaining remaining2)

--------------------------------------------------
-- * patternProperties
--------------------------------------------------

data PatternPropertiesInvalid err
    = PPInvalid err
    | PPAdditionalPropertiesInvalid (AdditionalPropertiesInvalid err)
    deriving (Eq, Show)

patternProperties
    :: forall err schema.
       (schema -> Value -> [Fail err])
    -> Bool
    -> Maybe (AdditionalProperties schema)
    -> HashMap Text schema
    -> HashMap Text Value
    -> [Fail (PatternPropertiesInvalid err)]
patternProperties _ False _ _ _ = mempty
patternProperties f _ mAdd patternPropertiesHm x =
       (fmap PPInvalid <$> ppFailures)
    <> (fmap PPAdditionalPropertiesInvalid <$> addFailures)
  where
    patternProps :: ([Fail err], Remaining)
    patternProps = patternAndUnmatched f patternPropertiesHm x

    (ppFailures, remaining) = patternProps

    addFailures :: [Fail (AdditionalPropertiesInvalid err)]
    addFailures =
        case mAdd of
            Nothing -> mempty
            Just a  -> additionalProperties f True a (_unRemaining remaining)

patternAndUnmatched
    :: forall err schema.
       (schema -> Value -> [Fail err])
    -> HashMap Text schema
    -> HashMap Text Value
    -> ([Fail err], Remaining)
patternAndUnmatched f patPropertiesHm x =
    (HM.foldlWithKey' runVals mempty perhapsMatches, remaining)
  where
    perhapsMatches :: HashMap Text (Value, [schema])
    perhapsMatches = HM.foldlWithKey' (matchingSchemas patPropertiesHm) mempty x
      where
        matchingSchemas
            :: HashMap Text schema
            -> HashMap Text (Value, [schema])
            -> Text
            -> Value
            -> HashMap Text (Value, [schema])
        matchingSchemas subSchemas acc k v =
            HM.insert k (v, HM.foldlWithKey' (checkKey k) mempty subSchemas) acc

        checkKey
            :: Text
            -> [schema]
            -> Text
            -> schema
            -> [schema]
        checkKey k acc r subSchema =
            case RE.compileM (encodeUtf8 r) mempty of
                Left _   -> acc
                Right re -> if k RE.=~ re
                                then pure subSchema <> acc
                                else acc

    runVals
        :: [Fail err]
        -> Text
        -> (Value, [schema])
        -> [Fail err]
    runVals acc k (v,subSchemas) =
        (subSchemas >>= (\schema -> prependToPath (AP.Token k) <$> f schema v))
        <> acc

    remaining :: Remaining
    remaining = Remaining . fmap fst . HM.filter (null . snd) $ perhapsMatches

--------------------------------------------------
-- * additionalProperties
--------------------------------------------------

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

data AdditionalPropertiesInvalid err
    = APBoolInvalid
    | APObjectInvalid err
    deriving (Eq, Show)

additionalProperties
    :: forall err schema.
       (schema -> Value -> [Fail err])
    -> Bool
    -> AdditionalProperties schema
    -> HashMap Text Value
    -> [Fail (AdditionalPropertiesInvalid err)]
additionalProperties _ False _ _ = mempty
additionalProperties f _ a x =
    case a of
        AdditionalPropertiesBool b ->
            ($> APBoolInvalid) <$> additionalPropertiesBool b x
        AdditionalPropertiesObject b ->
            fmap APObjectInvalid <$> additionalPropertiesObject f b x

additionalPropertiesBool
    :: Bool
    -> HashMap Text Value
    -> [Fail ()]
additionalPropertiesBool False x
    | HM.size x > 0 = pure $ Failure () (Bool False) mempty (Object x)
    | otherwise    = mempty
additionalPropertiesBool True _ = mempty

additionalPropertiesObject
    :: forall err schema.
       (schema -> Value -> [Fail err])
    -> schema
    -> HashMap Text Value
    -> [Fail err]
additionalPropertiesObject f schema x = HM.toList x >>= g
  where
    g :: (Text, Value) -> [Fail err]
    g (k,v) = prependToPath (AP.Token k) <$> f schema v
