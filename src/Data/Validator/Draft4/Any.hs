
module Data.Validator.Draft4.Any where

import           Control.Monad
import           Data.Aeson.Types         (Parser)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty       as N
import           Data.Maybe
import           Data.Scientific
import           Data.Set                 (Set)
import qualified Data.Set                 as S

import           Data.Validator.Failure
import           Data.Validator.Utils
import           Data.Validator.Reference (resolveFragment, resolveReference)
import           Import

-- For GHCs before 7.10:
import           Prelude hiding           (any, elem)

--------------------------------------------------
-- * $ref
--------------------------------------------------

-- | Will return 'Nothing' if the reference can't be resolved.
ref
  :: forall err schema. (FromJSON schema, ToJSON schema, Show schema)
  => Maybe Text
  -> (Maybe Text -> Maybe schema)
  -> (Maybe Text -> schema -> Value -> [Failure err])
  -> Text
  -> Value
  -> Maybe [Failure err]
ref scope getRef f reference x = do
  let (mUri, mFragment) = resolveReference scope reference
  schema <- getRef mUri
  s      <- resolveFragment mFragment schema
  Just (f mUri s x)

--------------------------------------------------
-- * enum
--------------------------------------------------

-- | From the spec:
-- <http://json-schema.org/latest/json-schema-validation.html#anchor76>
--
--  > The value of this keyword MUST be an array.
--  > This array MUST have at least one element.
--  > Elements in the array MUST be unique.
--
-- NOTE: We don't enforce the uniqueness constraint in the haskell code,
-- but we do in the 'FromJSON' instance.
newtype EnumVal
  = EnumVal { _unEnumVal :: NonEmpty Value }
  -- Given a choice, we'd prefer to enforce uniqueness through the type
  -- system over having at least one element. To use a 'Set' though we'd
  -- have to use 'OrdValue' here (there's no 'Ord' instance for plain Values)
  -- and we'd rather not make users mess with 'OrdValue'.
  deriving (Eq, Show)

instance FromJSON EnumVal where
  parseJSON v = checkUnique . _unNonEmpty' =<< parseJSON v
    where
      checkUnique :: NonEmpty Value -> Parser EnumVal
      checkUnique a
        | allUniqueValues' a = pure (EnumVal a)
        | otherwise = fail "All elements of the Enum validator must be unique."

instance ToJSON EnumVal where
  toJSON = toJSON . NonEmpty' . _unEnumVal

instance Arbitrary EnumVal where
  arbitrary = do
    xs <- traverse (const arbitraryValue) =<< (arbitrary :: Gen [()])
    case N.nonEmpty (toUnique xs) of
      Nothing -> EnumVal . pure <$> arbitraryValue
      Just ne -> pure (EnumVal ne)
    where
      toUnique :: [Value] -> [Value]
      toUnique = fmap _unOrdValue . S.toList . S.fromList . fmap OrdValue

enumVal :: EnumVal -> Value -> Maybe (Failure ())
enumVal (EnumVal vs) x
  | not (allUniqueValues' vs) = Nothing
  | x `elem` vs               = Nothing
  | otherwise                 =
    Just $ Failure () (toJSON (NonEmpty' vs)) mempty

--------------------------------------------------
-- * type
--------------------------------------------------

data TypeVal
  = TypeValString Text
  | TypeValArray (Set Text)
  deriving (Eq, Show)

instance FromJSON TypeVal where
  parseJSON v = fmap TypeValString (parseJSON v)
            <|> fmap TypeValArray (parseJSON v)

instance ToJSON TypeVal where
  toJSON (TypeValString t) = toJSON t
  toJSON (TypeValArray ts) = toJSON ts

instance Arbitrary TypeVal where
  arbitrary = oneof [ TypeValString <$> arbitraryText
                    , TypeValArray <$> arbitrarySetOfText
                    ]

typeVal :: TypeVal -> Value -> Maybe (Failure ())
typeVal (TypeValString t) x = isJsonType x (S.singleton t)
typeVal (TypeValArray ts) x = isJsonType x ts

isJsonType :: Value -> Set Text -> Maybe (Failure ())
isJsonType x ts
  | S.null (S.intersection okTypes ts) = Just (Failure () (toJSON ts) mempty)
  | otherwise                          = Nothing
  where
    okTypes :: Set Text
    okTypes =
      case x of
        Null       -> S.singleton "null"
        (Array _)  -> S.singleton "array"
        (Bool _)   -> S.singleton "boolean"
        (Object _) -> S.singleton "object"
        (String _) -> S.singleton "string"
        (Number y) ->
          case toBoundedInteger y :: Maybe Int of
            Nothing -> S.singleton "number"
            Just _  -> S.fromList ["number", "integer"]

--------------------------------------------------
-- * other
--------------------------------------------------

allOf
  :: (schema -> Value -> [Failure err])
  -> NonEmpty schema
  -> Value
  -> [Failure err]
allOf f subSchemas x = N.toList subSchemas >>= flip f x

anyOf
  :: ToJSON schema
  => (schema -> Value -> [Failure err])
  -> NonEmpty schema
  -> Value
  -> Maybe (Failure ())
anyOf f subSchemas x
  | any null (flip f x <$> subSchemas) = Nothing
  | otherwise = Just $ Failure () (toJSON (NonEmpty' subSchemas)) mempty

oneOf
  :: forall err schema. ToJSON schema
  => (schema -> Value -> [Failure err])
  -> NonEmpty schema
  -> Value
  -> Maybe (Failure ())
oneOf f subSchemas x
  | length successes == 1 = Nothing
  | otherwise             = Just $ Failure ()
                                           (toJSON (NonEmpty' subSchemas))
                                           mempty
  where
    successes :: [[Failure err]]
    successes = filter null $ flip f x <$> N.toList subSchemas

notVal
  :: ToJSON schema
  => (schema -> Value -> [Failure err])
  -> schema
  -> Value
  -> Maybe (Failure ())
notVal f schema x =
  case f schema x of
    [] -> Just (Failure () (toJSON schema) mempty)
    _  -> Nothing
