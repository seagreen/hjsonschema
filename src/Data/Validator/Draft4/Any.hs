{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Validator.Draft4.Any where

import           Import
-- Hiding is for GHCs before 7.10:
import           Prelude hiding           (any, elem)

import           Control.Monad
import           Data.Aeson.Types         (Parser)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty       as NE
import           Data.Maybe
import qualified Data.Scientific          as SCI
import           Data.Set                 (Set)
import qualified Data.Set                 as S

import           Data.Validator.Failure   (Fail(..))
import qualified Data.Validator.Utils     as UT
import           Data.Validator.Reference (URIBaseAndFragment,
                                           resolveFragment, resolveReference)

--------------------------------------------------
-- * $ref
--------------------------------------------------

data RefInvalid err
    = RefResolution
    | RefLoop
    | RefInvalid err
    deriving (Eq, Show)

newtype VisitedSchemas
    = VisitedSchemas { _unVisited :: [URIBaseAndFragment] }
    deriving (Eq, Show, Monoid)

ref
    :: forall err schema. (FromJSON schema, ToJSON schema)
    => VisitedSchemas
    -> Maybe Text
    -> (Maybe Text -> Maybe schema)
    -> (VisitedSchemas -> Maybe Text -> schema -> Value -> [Fail err])
    -> Text
    -> Value
    -> [Fail (RefInvalid err)]
ref visited scope getRef f reference x
    | (mUri, mFragment) `elem` _unVisited visited =
        pure $ Failure RefLoop (toJSON reference) mempty x
    | otherwise =
        case getRef mUri of
            Nothing     -> pure failureDNE
            Just schema ->
                case resolveFragment mFragment schema of
                    Nothing -> pure failureDNE
                    Just s  ->
                        let newVisited = (VisitedSchemas [(mUri, mFragment)]
                                      <> visited)
                        in fmap RefInvalid <$> f newVisited mUri s x
  where
    (mUri, mFragment) = resolveReference scope reference

    failureDNE :: Fail (RefInvalid err)
    failureDNE = Failure RefResolution (toJSON reference) mempty x

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
    parseJSON v = checkUnique . UT._unNonEmpty' =<< parseJSON v
      where
        checkUnique :: NonEmpty Value -> Parser EnumVal
        checkUnique a
            | UT.allUniqueValues' a = pure (EnumVal a)
            | otherwise = fail "All elements of the Enum validator must be unique."

instance ToJSON EnumVal where
    toJSON = toJSON . UT.NonEmpty' . _unEnumVal

instance Arbitrary EnumVal where
    arbitrary = do
        xs <- traverse (const UT.arbitraryValue) =<< (arbitrary :: Gen [()])
        case NE.nonEmpty (toUnique xs) of
            Nothing -> EnumVal . pure <$> UT.arbitraryValue
            Just ne -> pure (EnumVal ne)
      where
        toUnique :: [Value] -> [Value]
        toUnique = fmap UT._unOrdValue . S.toList . S.fromList . fmap UT.OrdValue

enumVal :: EnumVal -> Value -> Maybe (Fail ())
enumVal (EnumVal vs) x
    | not (UT.allUniqueValues' vs) = Nothing
    | x `elem` vs                  = Nothing
    | otherwise                    = Just $ Failure () (toJSON (UT.NonEmpty' vs))
                                                    mempty x

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
    arbitrary = oneof [ TypeValString <$> UT.arbitraryText
                      , TypeValArray <$> UT.arbitrarySetOfText
                      ]

setFromTypeVal :: TypeVal -> Set Text
setFromTypeVal (TypeValString t) = S.singleton t
setFromTypeVal (TypeValArray ts) = ts

typeVal :: TypeVal -> Value -> Maybe (Fail ())
typeVal tv x
    | S.null matches = Just (Failure () (toJSON tv) mempty x)
    | otherwise      = Nothing
  where
    -- There can be more than one match because a 'Value' can be both a
    -- @"number"@ and an @"integer"@.
    matches :: Set Text
    matches = S.intersection okTypes (setFromTypeVal tv)

    okTypes :: Set Text
    okTypes =
        case x of
            Null       -> S.singleton "null"
            (Array _)  -> S.singleton "array"
            (Bool _)   -> S.singleton "boolean"
            (Object _) -> S.singleton "object"
            (String _) -> S.singleton "string"
            (Number y) ->
                if SCI.isInteger y
                    then S.fromList ["number", "integer"]
                    else S.singleton "number"

--------------------------------------------------
-- * allOf
--------------------------------------------------

allOf
    :: (schema -> Value -> [Fail err])
    -> NonEmpty schema
    -> Value
    -> [Fail err]
allOf f subSchemas x = NE.toList subSchemas >>= flip f x

--------------------------------------------------
-- * anyOf
--------------------------------------------------

anyOf
    :: ToJSON schema
    => (schema -> Value -> [Fail err])
    -> NonEmpty schema
    -> Value
    -> Maybe (Fail ())
anyOf f subSchemas x
    | any null (flip f x <$> subSchemas) = Nothing
    | otherwise = Just $ Failure () (toJSON (UT.NonEmpty' subSchemas))
                                 mempty x

--------------------------------------------------
-- * oneOf
--------------------------------------------------

oneOf
    :: forall err schema. ToJSON schema
    => (schema -> Value -> [Fail err])
    -> NonEmpty schema
    -> Value
    -> Maybe (Fail ())
oneOf f subSchemas x
    | length successes == 1 = Nothing
    | otherwise = Just $ Failure () (toJSON (UT.NonEmpty' subSchemas)) mempty x
  where
    successes :: [[Fail err]]
    successes = filter null $ flip f x <$> NE.toList subSchemas

--------------------------------------------------
-- * not
--------------------------------------------------

notVal
    :: ToJSON schema
    => (schema -> Value -> [Fail err])
    -> schema
    -> Value
    -> Maybe (Fail ())
notVal f schema x =
    case f schema x of
        [] -> Just (Failure () (toJSON schema) mempty x)
        _  -> Nothing
