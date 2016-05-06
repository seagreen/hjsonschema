
module Data.Validator.Draft4.Array where

import           Control.Monad
import qualified Data.Aeson.Pointer     as P
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Text.Read              (readMaybe)

import           Data.Validator.Failure
import           Data.Validator.Utils   (allUniqueValues)
import           Import

-- | The spec requires "maxItems" to be non-negative.
maxItems :: Int -> Vector Value -> Maybe (Failure ())
maxItems n xs
  | n < 0           = Nothing
  | V.length xs > n = Just (Invalid () (toJSON n) mempty)
  | otherwise       = Nothing

-- | The spec requires "minItems" to be non-negative.
minItems :: Int -> Vector Value -> Maybe (Failure ())
minItems n xs
  | n < 0           = Nothing
  | V.length xs < n = Just (Invalid () (toJSON n) mempty)
  | otherwise       = Nothing

uniqueItems :: Bool -> Vector Value -> Maybe (Failure ())
uniqueItems True xs
  | allUniqueValues xs = Nothing
  | otherwise          = Just (Invalid () (Bool True) mempty)
uniqueItems False _ = Nothing

--------------------------------------------------
-- * items
--------------------------------------------------

data ItemsFailure err
  = Items err
  | AdditionalItemsBoolFailure
  | AdditionalItemsObjectFailure err
  deriving (Eq, Show)

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

items
  :: forall err schema.
     (schema -> Value -> [Failure err])
  -> Maybe (AdditionalItems schema)
  -> Items schema
  -> Vector Value
  -> [Failure (ItemsFailure err)]
items f _ (ItemsObject subSchema) xs = zip [0..] (V.toList xs) >>= g
  where
    g :: (Int, Value) -> [Failure (ItemsFailure err)]
    g (index,x) = modFailure Items
                . addToPath (P.Token (T.pack (show index)))
              <$> f subSchema x

items f mAdditional (ItemsArray subSchemas) xs = itemFailures
                                              <> additionalItemFailures
  where
    indexedValues :: [(Int, Value)]
    indexedValues = zip [0..] (V.toList xs)

    itemFailures :: [Failure (ItemsFailure err)]
    itemFailures = join (zipWith g subSchemas indexedValues)
      where
        g :: schema -> (Int, Value) -> [Failure (ItemsFailure err)]
        g schema (index,x) = modFailure Items
                           . addToPath (P.Token (T.pack (show index)))
                         <$> f schema x

    additionalItemFailures :: [Failure (ItemsFailure err)]
    additionalItemFailures =
      case mAdditional of
        Nothing  -> mempty
        Just adi -> modFailure correctName
                  . correctIndexes
                <$> additionalItems f adi extras
      where
        -- It's not great that we convert back to Vector again.
        extras :: Vector Value
        extras =
          V.fromList . fmap snd . drop (length subSchemas) $ indexedValues

        -- Since 'additionalItems' only sees part of the array, but starts
        -- indexing from zero, we need to modify the paths it reports to
        -- represent invalid data so they actually represent the correct
        -- offsets.
        correctIndexes
          :: Failure (AdditionalItemsFailure err)
          -> Failure (AdditionalItemsFailure err)
        correctIndexes (Invalid a b c) = Invalid a b (fixIndex c)
          where
            fixIndex :: P.Pointer -> P.Pointer
            fixIndex (P.Pointer (tok:toks)) =
              case readMaybe . T.unpack . P._unToken $ tok of
                Nothing -> P.Pointer $ tok:toks
                Just n  -> P.Pointer $
                  (P.Token . T.pack . show $ n + length subSchemas):toks
            fixIndex (P.Pointer []) = P.Pointer []

        correctName :: AdditionalItemsFailure err -> ItemsFailure err
        correctName AdditionalBoolFailure = AdditionalItemsBoolFailure
        correctName (AdditionalObjectFailure err) =
          AdditionalItemsObjectFailure err

--------------------------------------------------
-- * additionalItems
--------------------------------------------------

data AdditionalItemsFailure err
  = AdditionalBoolFailure
  | AdditionalObjectFailure err
  deriving (Eq, Show)

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

additionalItems
  :: forall err schema.
     (schema -> Value -> [Failure err])
  -> AdditionalItems schema
  -> Vector Value
  -> [Failure (AdditionalItemsFailure err)]
additionalItems _ (AdditionalBool b) xs
  | b               = mempty
  | V.length xs > 0 = pure (Invalid AdditionalBoolFailure (Bool b) mempty)
  | otherwise       = mempty
additionalItems f (AdditionalObject subSchema) xs =
  zip [0..] (V.toList xs) >>= g
  where
    g :: (Int, Value) -> [Failure (AdditionalItemsFailure err)]
    g (index,x) = modFailure AdditionalObjectFailure
                . addToPath (P.Token (T.pack (show index)))
              <$> f subSchema x
