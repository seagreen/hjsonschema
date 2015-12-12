{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Validator.Draft4.Object
  ( module Data.Validator.Draft4.Object
  , module Data.Validator.Draft4.Object.Properties
  ) where

import           Data.Aeson.Types                        (Parser)
import qualified Data.HashMap.Strict                     as H
import           Data.Set                                (Set)
import qualified Data.Set                                as S
import qualified Data.Text                               as T

import           Data.Validator.Draft4.Object.Properties
import           Data.Validator.Failure
import           Data.Validator.Utils
import           Import

-- For GHCs before 7.10:
import           Prelude                                 hiding (all, concat,
                                                          foldl)

-- | The spec requires "maxProperties" to be non-negative.
maxProperties :: Int -> HashMap Text Value -> Maybe (Failure ())
maxProperties n x
  | n < 0        = Nothing
  | H.size x > n = Just (Failure () (toJSON n) mempty)
  | otherwise    = Nothing

-- | The spec requires "minProperties" to be non-negative.
minProperties :: Int -> HashMap Text Value -> Maybe (Failure ())
minProperties n x
  | n < 0        = Nothing
  | H.size x < n = Just (Failure () (toJSON n) mempty)
  | otherwise    = Nothing

--------------------------------------------------
-- * required
--------------------------------------------------

-- | From the spec:
--
-- > The value of this keyword MUST be an array.
-- > This array MUST have at least one element.
-- > Elements of this array MUST be strings, and MUST be unique.
--
-- We don't enfore that 'Required' has at least one element in the
-- haskell code, but we do in the 'FromJSON' instance.
newtype Required
  = Required { _unRequired :: Set Text }
  deriving (Eq, Show, ToJSON)

instance FromJSON Required where
  parseJSON v = checkUnique =<< checkSize =<< parseJSON v
    where
      checkSize :: [Text] -> Parser [Text]
      checkSize a
        | null a    = fail "Required validator must not be empty."
        | otherwise = pure a

      checkUnique :: [Text] -> Parser Required
      checkUnique a =
        let b = S.fromList a
        -- NOTE: Can use length instead of S.size in GHC 7.10 or later.
        in if length a == S.size b
          then pure (Required b)
          else fail "All elements of the Required validator must be unique."

instance Arbitrary Required where
  arbitrary = do
    x  <- arbitraryText -- Guarantee at least one element.
    xs <- (fmap.fmap) T.pack arbitrary
    pure . Required . S.fromList $ x:xs

required :: Required -> HashMap Text Value -> Maybe (Failure ())
required (Required ts) x
  -- NOTE: When we no longer need to support GHCs before 7.10
  -- we can use null from Prelude throughout the library
  -- instead of specialized versions.
  | S.null ts                  = Nothing
  | H.null (H.difference hm x) = Nothing
  | otherwise                  = Just (Failure () (toJSON ts) mempty)
  where
    hm :: HashMap Text Bool
    hm = foldl (\b a -> H.insert a True b) mempty ts

--------------------------------------------------
-- * dependencies
--------------------------------------------------

data DependencyFailure err
  = SchemaDependencyFailure err
  | PropertyDependencyFailure
  deriving (Eq, Show)

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
dependencies
  :: forall err schema. (schema -> Value -> [Failure err])
  -> HashMap Text (Dependency schema)
  -> HashMap Text Value
  -> [Failure (DependencyFailure err)]
dependencies f hm x = concat . fmap (uncurry g) . H.toList $ hm
  where
    g :: Text -> Dependency schema -> [Failure (DependencyFailure err)]
    g k (SchemaDependency schema)
      | H.member k x =
        modFailure SchemaDependencyFailure <$> f schema (Object x)
      | otherwise    = mempty
    g k (PropertyDependency ts)
      | H.member k x && not allPresent =
        pure $ Failure PropertyDependencyFailure
                       (toJSON (H.singleton k ts))
                       mempty
      | otherwise = mempty
      where
        allPresent :: Bool
        allPresent = all (`H.member` x) ts
