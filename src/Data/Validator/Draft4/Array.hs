
module Data.Validator.Draft4.Array where

import           Import
import           Prelude

import           Control.Monad
import qualified Data.Aeson.Pointer     as AP
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Text.Read              (readMaybe)

import           Data.Validator.Failure (Fail(..), prependToPath)
import           Data.Validator.Utils   (allUniqueValues)

--------------------------------------------------
-- * maxItems
--------------------------------------------------

-- | The spec requires @"maxItems"@ to be non-negative.
maxItems :: Int -> Vector Value -> Maybe (Fail ())
maxItems n xs
    | n < 0           = Nothing
    | V.length xs > n = Just (Failure () (toJSON n) mempty (Array xs))
    | otherwise       = Nothing

--------------------------------------------------
-- * minItems
--------------------------------------------------

-- | The spec requires @"minItems"@ to be non-negative.
minItems :: Int -> Vector Value -> Maybe (Fail ())
minItems n xs
    | n < 0           = Nothing
    | V.length xs < n = Just (Failure () (toJSON n) mempty (Array xs))
    | otherwise       = Nothing

--------------------------------------------------
-- * uniqueItems
--------------------------------------------------

uniqueItems :: Bool -> Vector Value -> Maybe (Fail ())
uniqueItems True xs
   | allUniqueValues xs = Nothing
   | otherwise          = Just (Failure () (Bool True) mempty (Array xs))
uniqueItems False _ = Nothing

--------------------------------------------------
-- * items
--------------------------------------------------

data Items schema
    = ItemsObject schema
    | ItemsArray [schema]
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (Items schema) where
    parseJSON v = fmap ItemsObject (parseJSON v)
              <|> fmap ItemsArray (parseJSON v)

instance ToJSON schema => ToJSON (Items schema) where
    toJSON (ItemsObject hm)     = toJSON hm
    toJSON (ItemsArray schemas) = toJSON schemas

instance Arbitrary schema => Arbitrary (Items schema) where
    arbitrary = oneof [ ItemsObject <$> arbitrary
                      , ItemsArray <$> arbitrary
                      ]

data ItemsInvalid err
    = Items err
    | AdditionalItemsBoolInvalid
    | AdditionalItemsObjectInvalid err
    deriving (Eq, Show)

items
    :: forall err schema.
       (schema -> Value -> [Fail err])
    -> Maybe (AdditionalItems schema)
    -> Items schema
    -> Vector Value
    -> [Fail (ItemsInvalid err)]
items f _ (ItemsObject subSchema) xs =
    zip [0..] (V.toList xs) >>= g
  where
    g :: (Int, Value) -> [Fail (ItemsInvalid err)]
    g (index,x) = fmap Items
                . prependToPath (AP.Token (T.pack (show index)))
              <$> f subSchema x

items f mAdditional (ItemsArray subSchemas) xs =
    itemFailures <> additionalItemFailures
  where
    indexedValues :: [(Int, Value)]
    indexedValues = zip [0..] (V.toList xs)

    itemFailures :: [Fail (ItemsInvalid err)]
    itemFailures = join (zipWith g subSchemas indexedValues)
      where
        g :: schema -> (Int, Value) -> [Fail (ItemsInvalid err)]
        g schema (index,x) = fmap Items
                           . prependToPath (AP.Token (T.pack (show index)))
                         <$> f schema x

    additionalItemFailures :: [Fail (ItemsInvalid err)]
    additionalItemFailures =
        case mAdditional of
            Nothing  -> mempty
            Just adi -> fmap correctName
                      . correctIndexes
                    <$> additionalItems f adi extras
      where
        -- It's not great that we convert back to Vector again.
        extras :: Vector Value
        extras = V.fromList . fmap snd
               . drop (length subSchemas) $ indexedValues

        -- Since 'additionalItems' only sees part of the array, but starts
        -- indexing from zero, we need to modify the paths it reports to
        -- represent invalid data so they actually represent the correct
        -- offsets.
        correctIndexes
          :: Fail (AdditionalItemsInvalid err)
          -> Fail (AdditionalItemsInvalid err)
        correctIndexes (Failure a b c d) = Failure a b (fixIndex c) d
          where
            fixIndex :: AP.Pointer -> AP.Pointer
            fixIndex (AP.Pointer (tok:toks)) =
                case readMaybe . T.unpack . AP._unToken $ tok of
                    Nothing -> AP.Pointer $ tok:toks
                    Just n  -> AP.Pointer $
                        (AP.Token . T.pack . show $ n + length subSchemas):toks
            fixIndex (AP.Pointer []) = AP.Pointer []

        correctName :: AdditionalItemsInvalid err -> ItemsInvalid err
        correctName AdditionalBoolInvalid = AdditionalItemsBoolInvalid
        correctName (AdditionalObjectInvalid err) =
            AdditionalItemsObjectInvalid err

--------------------------------------------------
-- * additionalItems
--------------------------------------------------

data AdditionalItems schema
    = AdditionalBool Bool
    | AdditionalObject schema
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (AdditionalItems schema) where
    parseJSON v = fmap AdditionalBool (parseJSON v)
              <|> fmap AdditionalObject (parseJSON v)

instance ToJSON schema => ToJSON (AdditionalItems schema) where
    toJSON (AdditionalBool b)    = toJSON b
    toJSON (AdditionalObject hm) = toJSON hm

instance Arbitrary schema => Arbitrary (AdditionalItems schema) where
    arbitrary = oneof [ AdditionalBool <$> arbitrary
                      , AdditionalObject <$> arbitrary
                      ]

data AdditionalItemsInvalid err
    = AdditionalBoolInvalid
    | AdditionalObjectInvalid err
    deriving (Eq, Show)

additionalItems
    :: forall err schema.
       (schema -> Value -> [Fail err])
    -> AdditionalItems schema
    -> Vector Value
    -> [Fail (AdditionalItemsInvalid err)]
additionalItems _ (AdditionalBool b) xs
    | b               = mempty
    | V.length xs > 0 = pure (Failure AdditionalBoolInvalid (Bool b)
                                      mempty (toJSON xs))
    | otherwise       = mempty
additionalItems f (AdditionalObject subSchema) xs =
    zip [0..] (V.toList xs) >>= g
  where
    g :: (Int, Value) -> [Fail (AdditionalItemsInvalid err)]
    g (index,x) = fmap AdditionalObjectInvalid
                . prependToPath (AP.Token (T.pack (show index)))
              <$> f subSchema x
