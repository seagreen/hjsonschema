
module JSONSchema.Validator.Draft4.Array where

import           Import

import qualified Data.List.NonEmpty         as NE
import qualified Data.Vector                as V
import qualified JSONPointer                as JP

import           JSONSchema.Validator.Utils (allUniqueValues)

--------------------------------------------------
-- * maxItems
--------------------------------------------------

newtype MaxItems
    = MaxItems { _unMaxItems :: Int }
    deriving (Eq, Show)

instance FromJSON MaxItems where
    parseJSON = withObject "MaxItems" $ \o ->
        MaxItems <$> o .: "maxItems"

data MaxItemsInvalid
    = MaxItemsInvalid MaxItems (Vector Value)
    deriving (Eq, Show)

-- | The spec requires @"maxItems"@ to be non-negative.
maxItemsVal :: MaxItems -> Vector Value -> Maybe MaxItemsInvalid
maxItemsVal a@(MaxItems n) xs
    | n < 0           = Nothing
    | V.length xs > n = Just (MaxItemsInvalid a xs)
    | otherwise       = Nothing

--------------------------------------------------
-- * minItems
--------------------------------------------------

newtype MinItems
    = MinItems { _unMinItems :: Int }
    deriving (Eq, Show)

instance FromJSON MinItems where
    parseJSON = withObject "MinItems" $ \o ->
        MinItems <$> o .: "minItems"

data MinItemsInvalid
    = MinItemsInvalid MinItems (Vector Value)
    deriving (Eq, Show)

-- | The spec requires @"minItems"@ to be non-negative.
minItemsVal :: MinItems -> Vector Value -> Maybe MinItemsInvalid
minItemsVal a@(MinItems n) xs
    | n < 0           = Nothing
    | V.length xs < n = Just (MinItemsInvalid a xs)
    | otherwise       = Nothing

--------------------------------------------------
-- * uniqueItems
--------------------------------------------------

newtype UniqueItems
    = UniqueItems { _unUniqueItems :: Bool }
    deriving (Eq, Show)

instance FromJSON UniqueItems where
    parseJSON = withObject "UniqueItems" $ \o ->
        UniqueItems <$> o .: "uniqueItems"

data UniqueItemsInvalid
    = UniqueItemsInvalid (Vector Value)
    deriving (Eq, Show)

uniqueItemsVal :: UniqueItems -> Vector Value -> Maybe UniqueItemsInvalid
uniqueItemsVal (UniqueItems True) xs
   | allUniqueValues xs = Nothing
   | otherwise          = Just (UniqueItemsInvalid xs)
uniqueItemsVal (UniqueItems False) _ = Nothing

--------------------------------------------------
-- * items
--------------------------------------------------

data ItemsRelated schema = ItemsRelated
    { _irItems      :: Maybe (Items schema)
    , _irAdditional :: Maybe (AdditionalItems schema)
    } deriving (Eq, Show)

instance FromJSON schema => FromJSON (ItemsRelated schema) where
    parseJSON = withObject "ItemsRelated" $ \o -> ItemsRelated
        <$> o .:! "items"
        <*> o .:! "additionalItems"

emptyItems :: ItemsRelated schema
emptyItems = ItemsRelated
    { _irItems      = Nothing
    , _irAdditional = Nothing
    }

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

data ItemsRelatedInvalid err
    = IRInvalidItems      (ItemsInvalid err)
    | IRInvalidAdditional (AdditionalItemsInvalid err)
    deriving (Eq, Show)

data ItemsInvalid err
    = ItemsObjectInvalid (NonEmpty (JP.Index, NonEmpty err))
    | ItemsArrayInvalid  (NonEmpty (JP.Index, NonEmpty err))
    deriving (Eq, Show)

-- | @"additionalItems"@ only matters if @"items"@ exists
-- and is a JSON Array.
itemsRelatedVal
    :: forall err schema.
       (schema -> Value -> [err])
    -> ItemsRelated schema
    -> Vector Value
    -> [ItemsRelatedInvalid err] -- NOTE: 'Data.These' would help here.
itemsRelatedVal f a xs =
    let (itemsFailure, remaining) = case _irItems a of
                                        Nothing -> (Nothing, mempty)
                                        Just b  -> itemsVal f b xs
        additionalFailure = (\b -> additionalItemsVal f b remaining)
                        =<< _irAdditional a
    in catMaybes [ IRInvalidItems <$> itemsFailure
                 , IRInvalidAdditional <$> additionalFailure
                 ]

-- | Internal.
--
-- This is because 'itemsRelated' handles @"items"@ validation.
itemsVal
    :: forall err schema.
       (schema -> Value -> [err])
    -> Items schema
    -> Vector Value
    -> (Maybe (ItemsInvalid err), [(JP.Index, Value)])
       -- ^ The second item in the tuple is the elements of the original
       -- JSON Array still remaining to be checked by @"additionalItems"@.
itemsVal f a xs =
    case a of
        ItemsObject subSchema ->
            case NE.nonEmpty (mapMaybe (validateElem subSchema) indexed) of
                Nothing   -> (Nothing, mempty)
                Just errs -> (Just (ItemsObjectInvalid errs), mempty)
        ItemsArray subSchemas ->
            let remaining = drop (length subSchemas) indexed
                res = catMaybes (zipWith validateElem subSchemas indexed)
            in case NE.nonEmpty res of
                Nothing   -> (Nothing, remaining)
                Just errs -> (Just (ItemsArrayInvalid errs), remaining)
  where
    indexed :: [(JP.Index, Value)]
    indexed = zip (JP.Index <$> [0..]) (V.toList xs)

    validateElem
        :: schema
        -> (JP.Index, Value)
        -> Maybe (JP.Index, NonEmpty err)
    validateElem schema (index,x) = (index,) <$> NE.nonEmpty (f schema x)

--------------------------------------------------
-- * additionalItems
--------------------------------------------------

data AdditionalItems schema
    = AdditionalBool   Bool
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
    = AdditionalItemsBoolInvalid   (NonEmpty (JP.Index, Value))
    | AdditionalItemsObjectInvalid (NonEmpty (JP.Index, NonEmpty err))
    deriving (Eq, Show)

-- | Internal.
--
-- This is because 'itemsRelated' handles @"additionalItems"@ validation.
additionalItemsVal
    :: forall err schema.
       (schema -> Value -> [err])
    -> AdditionalItems schema
    -> [(JP.Index, Value)]
       -- ^ The elements remaining to validate after the ones covered by
       -- @"items"@ have been removed.
    -> Maybe (AdditionalItemsInvalid err)
additionalItemsVal _ (AdditionalBool True) _ = Nothing
additionalItemsVal _ (AdditionalBool False) xs =
    AdditionalItemsBoolInvalid <$> NE.nonEmpty xs
additionalItemsVal f (AdditionalObject subSchema) xs =
    let res = mapMaybe
                  (\(index,x) -> (index,) <$> NE.nonEmpty (f subSchema x))
                  xs
    in AdditionalItemsObjectInvalid <$> NE.nonEmpty res
