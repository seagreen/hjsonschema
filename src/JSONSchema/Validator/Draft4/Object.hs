
module JSONSchema.Validator.Draft4.Object
  ( module JSONSchema.Validator.Draft4.Object
  , module JSONSchema.Validator.Draft4.Object.Properties
  ) where

import           Import

import qualified Data.HashMap.Strict                           as HM
import qualified Data.List.NonEmpty                            as NE
import           Data.Set                                      (Set)
import qualified Data.Set                                      as Set
import qualified Data.Text                                     as T

import           JSONSchema.Validator.Draft4.Object.Properties
import           JSONSchema.Validator.Utils

--------------------------------------------------
-- * maxProperties
--------------------------------------------------

newtype MaxProperties
    = MaxProperties { _unMaxProperties :: Int }
    deriving (Eq, Show)

instance FromJSON MaxProperties where
    parseJSON = withObject "MaxProperties" $ \o ->
        MaxProperties <$> o .: "maxProperties"

data MaxPropertiesInvalid
    = MaxPropertiesInvalid MaxProperties (HashMap Text Value)
    deriving (Eq, Show)

-- | The spec requires @"maxProperties"@ to be non-negative.
maxPropertiesVal
    :: MaxProperties
    -> HashMap Text Value
    -> Maybe MaxPropertiesInvalid
maxPropertiesVal a@(MaxProperties n) x
    | n < 0         = Nothing
    | HM.size x > n = Just (MaxPropertiesInvalid a x)
    | otherwise     = Nothing

--------------------------------------------------
-- * minProperties
--------------------------------------------------

newtype MinProperties
    = MinProperties { _unMinProperties :: Int }
    deriving (Eq, Show)

instance FromJSON MinProperties where
    parseJSON = withObject "MinProperties" $ \o ->
        MinProperties <$> o .: "minProperties"

data MinPropertiesInvalid
    = MinPropertiesInvalid MinProperties (HashMap Text Value)
    deriving (Eq, Show)

-- | The spec requires @"minProperties"@ to be non-negative.
minPropertiesVal
    :: MinProperties
    -> HashMap Text Value
    -> Maybe MinPropertiesInvalid
minPropertiesVal a@(MinProperties n) x
    | n < 0         = Nothing
    | HM.size x < n = Just (MinPropertiesInvalid a x)
    | otherwise     = Nothing

--------------------------------------------------
-- * required
--------------------------------------------------

-- | From the spec:
--
-- > The value of this keyword MUST be an array.
-- > This array MUST have at least one element.
-- > Elements of this array MUST be strings, and MUST be unique.
newtype Required
    = Required { _unRequired :: Set Text }
    deriving (Eq, Show)

instance FromJSON Required where
    parseJSON = withObject "Required" $ \o ->
        Required <$> o .: "required"

instance Arbitrary Required where
    arbitrary = do
        x  <- arbitraryText -- Guarantee at least one element.
        xs <- (fmap.fmap) T.pack arbitrary
        pure . Required . Set.fromList $ x:xs

data RequiredInvalid
    = RequiredInvalid Required (Set Text) (HashMap Text Value)
    deriving (Eq, Show)

requiredVal :: Required -> HashMap Text Value -> Maybe RequiredInvalid
requiredVal r@(Required ts) x
    -- NOTE: When we no longer need to support GHCs before 7.10
    -- we can use null from Prelude throughout the library
    -- instead of specialized versions.
    | Set.null ts        = Nothing
    | Set.null leftovers = Nothing
    | otherwise          = Just (RequiredInvalid r leftovers x)
  where
    leftovers :: Set Text
    leftovers =
        Set.difference -- Items of the first set not in the second.
            ts
            (Set.fromList (HM.keys x))

--------------------------------------------------
-- * dependencies
--------------------------------------------------

newtype DependenciesValidator schema
    = DependenciesValidator
        { _unDependenciesValidator :: HashMap Text (Dependency schema) }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (DependenciesValidator schema) where
    parseJSON = withObject "DependenciesValidator" $ \o ->
        DependenciesValidator <$> o .: "dependencies"

data Dependency schema
    = SchemaDependency schema
    | PropertyDependency (Set Text)
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (Dependency schema) where
    parseJSON v = fmap SchemaDependency (parseJSON v)
              <|> fmap PropertyDependency (parseJSON v)

instance ToJSON schema => ToJSON (Dependency schema) where
    toJSON (SchemaDependency schema) = toJSON schema
    toJSON (PropertyDependency ts)   = toJSON ts

instance Arbitrary schema => Arbitrary (Dependency schema) where
    arbitrary = oneof [ SchemaDependency <$> arbitrary
                      , PropertyDependency <$> arbitrarySetOfText
                      ]

data DependencyMemberInvalid err
    = SchemaDepInvalid   (NonEmpty err)
    | PropertyDepInvalid (Set Text) (HashMap Text Value)
    deriving (Eq, Show)

newtype DependenciesInvalid err
    = DependenciesInvalid (HashMap Text (DependencyMemberInvalid err))
    deriving (Eq, Show)

-- | From the spec:
-- <http://json-schema.org/latest/json-schema-validation.html#anchor70>
--
-- > This keyword's value MUST be an object.
-- > Each value of this object MUST be either an object or an array.
-- >
-- > If the value is an object, it MUST be a valid JSON Schema.
-- > This is called a schema dependency.
-- >
-- > If the value is an array, it MUST have at least one element.
-- > Each element MUST be a string, and elements in the array MUST be unique.
-- > This is called a property dependency.
dependenciesVal
    :: forall err schema.
       (schema -> Value -> [err])
    -> DependenciesValidator schema
    -> HashMap Text Value
    -> Maybe (DependenciesInvalid err)
dependenciesVal f (DependenciesValidator hm) x =
    let res = HM.mapMaybeWithKey g hm
    in if HM.null res
        then Nothing
        else Just (DependenciesInvalid res)
    where
      g :: Text -> Dependency schema -> Maybe (DependencyMemberInvalid err)
      g k (SchemaDependency schema)
          | HM.member k x = SchemaDepInvalid
                        <$> NE.nonEmpty (f schema (Object x))
          | otherwise = Nothing
      g k (PropertyDependency ts)
          | HM.member k x && not allPresent = Just (PropertyDepInvalid ts x)
          | otherwise                       = Nothing
        where
          allPresent :: Bool
          allPresent = all (`HM.member` x) ts
