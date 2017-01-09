
module JSONSchema.Validator.Draft4.Any where

import           Import

import           Data.List.NonEmpty             (NonEmpty((:|)))
import qualified Data.List.NonEmpty             as NE
import qualified Data.Scientific                as SCI
import           Data.Semigroup                 (Semigroup) -- for older GHCs
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import qualified JSONPointer                    as JP

import qualified JSONSchema.Validator.Utils     as UT
import           JSONSchema.Validator.Reference (URIBaseAndFragment,
                                                 resolveFragment,
                                                 resolveReference)

--------------------------------------------------
-- * $ref
--------------------------------------------------

newtype Ref
    = Ref { _unRef :: Text }
    deriving (Eq, Show)

instance FromJSON Ref where
    parseJSON = withObject "Ref" $ \o ->
        Ref <$> o .: "$ref"

data RefInvalid err
    = RefResolution        Text
      -- ^ Indicates a reference that failed to resolve.
      --
      -- NOTE: The language agnostic test suite doesn't specify if this should
      -- cause a validation error or should allow data to pass. We choose to
      -- return a validation error.
      --
      -- Also note that ideally we would enforce in the type system that any
      -- failing references be dealt with before valididation. Then this could
      -- be removed entirely.
    | RefPointerResolution Text
    | RefLoop              Text VisitedSchemas URIBaseAndFragment
    | RefInvalid           Text Value (NonEmpty err)
      -- ^ 'Text' is the URI and 'Value' is the linked schema.
    deriving (Eq, Show)

newtype VisitedSchemas
    = VisitedSchemas { _unVisited :: [URIBaseAndFragment] }
    deriving (Eq, Show, Semigroup, Monoid)

refVal
    :: forall err schema. (FromJSON schema, ToJSON schema)
    => VisitedSchemas
    -> Maybe Text
    -> (Maybe Text -> Maybe schema)
    -> (VisitedSchemas -> Maybe Text -> schema -> Value -> [err])
    -> Ref
    -> Value
    -> Maybe (RefInvalid err)
refVal visited scope getRef f (Ref reference) x
    | (mURI, mFragment) `elem` _unVisited visited =
        Just (RefLoop reference visited (mURI, mFragment))
    | otherwise =
        case getRef mURI of
            Nothing     -> Just (RefResolution reference)
            Just schema ->
                case resolveFragment mFragment schema of
                    Nothing -> Just (RefPointerResolution reference)
                    Just s  ->
                        let newVisited = (VisitedSchemas [(mURI, mFragment)]
                                      <> visited)
                            errs = f newVisited mURI s x
                        in RefInvalid reference (toJSON schema)
                            <$> NE.nonEmpty errs
  where
    (mURI, mFragment) = resolveReference scope reference

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
newtype EnumValidator
    = EnumValidator { _unEnumValidator :: NonEmpty Value }
    -- Given a choice, we'd prefer to enforce uniqueness through the type
    -- system over having at least one element. To use a 'Set' though we'd
    -- have to use 'OrdValue' here (there's no 'Ord' instance for plain Values)
    -- and we'd rather not make users mess with 'OrdValue'.
    deriving (Eq, Show)

instance FromJSON EnumValidator where
    parseJSON = withObject "EnumValidator" $ \o ->
        EnumValidator <$> o .: "enum"

instance Arbitrary EnumValidator where
    arbitrary = do
        xs <- (fmap.fmap) UT._unArbitraryValue arbitrary
        case NE.nonEmpty (toUnique xs) of
            Nothing -> EnumValidator . pure . UT._unArbitraryValue <$> arbitrary
            Just ne -> pure (EnumValidator ne)
      where
        toUnique :: [Value] -> [Value]
        toUnique = fmap UT._unOrdValue . S.toList . S.fromList . fmap UT.OrdValue

data EnumInvalid
    = EnumInvalid EnumValidator Value
    deriving (Eq, Show)

enumVal :: EnumValidator -> Value -> Maybe EnumInvalid
enumVal a@(EnumValidator vs) x
    | not (UT.allUniqueValues' vs) = Nothing
    | x `elem` vs                  = Nothing
    | otherwise                    = Just $ EnumInvalid a x

--------------------------------------------------
-- * type
--------------------------------------------------

-- | This is separate from 'TypeValidator' so that 'TypeValidator' can
-- be used to write 'JSONSchema.Draft4.Schema.Schema' without
-- messing up the 'FromJSON' instance of that data type.
newtype TypeContext
    = TypeContext { _unTypeContext :: TypeValidator }
    deriving (Eq, Show)

instance FromJSON TypeContext where
    parseJSON = withObject "TypeContext" $ \o ->
        TypeContext <$> o .: "type"

data TypeValidator
    = TypeValidatorString Text
    | TypeValidatorArray  (Set Text)
    deriving (Eq, Show)

instance FromJSON TypeValidator where
    parseJSON v = fmap TypeValidatorString (parseJSON v)
              <|> fmap TypeValidatorArray (parseJSON v)

instance ToJSON TypeValidator where
    toJSON (TypeValidatorString t) = toJSON t
    toJSON (TypeValidatorArray ts) = toJSON ts

instance Arbitrary TypeValidator where
    arbitrary = oneof [ TypeValidatorString <$> UT.arbitraryText
                      , TypeValidatorArray <$> UT.arbitrarySetOfText
                      ]

data TypeValidatorInvalid
    = TypeValidatorInvalid TypeValidator Value
    deriving (Eq, Show)

typeVal :: TypeContext -> Value -> Maybe TypeValidatorInvalid
typeVal (TypeContext tv) x
    | S.null matches = Just (TypeValidatorInvalid tv x)
    | otherwise      = Nothing
  where
    -- There can be more than one match because a 'Value' can be both a
    -- @"number"@ and an @"integer"@.
    matches :: Set Text
    matches = S.intersection okTypes (setFromTypeValidator tv)

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

-- | Internal.
setFromTypeValidator :: TypeValidator -> Set Text
setFromTypeValidator (TypeValidatorString t) = S.singleton t
setFromTypeValidator (TypeValidatorArray ts) = ts

--------------------------------------------------
-- * allOf
--------------------------------------------------

newtype AllOf schema
    = AllOf { _unAllOf :: NonEmpty schema }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (AllOf schema) where
    parseJSON = withObject "AllOf" $ \o ->
        AllOf <$> o .: "allOf"

newtype AllOfInvalid err
    = AllOfInvalid (NonEmpty (JP.Index, NonEmpty err))
    deriving (Eq, Show)

allOfVal
    :: forall err schema.
       (schema -> Value -> [err])
    -> AllOf schema
    -> Value
    -> Maybe (AllOfInvalid err)
allOfVal f (AllOf subSchemas) x = AllOfInvalid <$> NE.nonEmpty failures
  where
    perhapsFailures :: [(JP.Index, [err])]
    perhapsFailures = zip (JP.Index <$> [0..])
                          (flip f x <$> NE.toList subSchemas)

    failures :: [(JP.Index, NonEmpty err)]
    failures = mapMaybe (traverse NE.nonEmpty) perhapsFailures

--------------------------------------------------
-- * anyOf
--------------------------------------------------

newtype AnyOf schema
    = AnyOf { _unAnyOf :: NonEmpty schema }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (AnyOf schema) where
    parseJSON = withObject "AnyOf" $ \o ->
        AnyOf <$> o .: "anyOf"

newtype AnyOfInvalid err
    = AnyOfInvalid (NonEmpty (JP.Index, NonEmpty err))
    deriving (Eq, Show)

anyOfVal
    :: forall err schema.
       (schema -> Value -> [err])
    -> AnyOf schema
    -> Value
    -> Maybe (AnyOfInvalid err)
anyOfVal f (AnyOf subSchemas) x
    -- Replace with @null@ once we drop GHC 7.8:
    | any (((==) 0 . length) . snd) perhapsFailures = Nothing
    | otherwise = AnyOfInvalid <$> NE.nonEmpty failures
  where
    perhapsFailures :: [(JP.Index, [err])]
    perhapsFailures = zip (JP.Index <$> [0..])
                          (flip f x <$> NE.toList subSchemas)

    failures :: [(JP.Index, NonEmpty err)]
    failures = mapMaybe (traverse NE.nonEmpty) perhapsFailures

--------------------------------------------------
-- * oneOf
--------------------------------------------------

newtype OneOf schema
    = OneOf { _unOneOf :: NonEmpty schema }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (OneOf schema) where
    parseJSON = withObject "OneOf" $ \o ->
        OneOf <$> o .: "oneOf"

data OneOfInvalid err
    = TooManySuccesses (NonEmpty (JP.Index, Value)) Value
      -- ^ The NonEmpty lists contains tuples whose contents
      -- are the index of a schema that validated the data
      -- and the contents of that schema.
    | NoSuccesses      (NonEmpty (JP.Index, NonEmpty err)) Value
      -- ^ The NonEmpty lists contains tuples whose contents
      -- are the index of a schema that failed to validate the data
      -- and the failures it produced.
    deriving (Eq, Show)

oneOfVal
    :: forall err schema. ToJSON schema
    => (schema -> Value -> [err])
    -> OneOf schema
    -> Value
    -> Maybe (OneOfInvalid err)
oneOfVal f (OneOf (firstSubSchema :| otherSubSchemas)) x =
    -- Producing the NonEmpty lists needed by the error constructors
    -- is a little tricky. If we had a partition function like this
    -- it might help:
    -- @
    -- (a -> Either b c) -> NonEmpty a -> Either (NonEmpty b, [c])
    --                                           ([b], NonEmpty c)
    -- @
    case (firstSuccess, otherSuccesses) of
        (Right _, Nothing)        -> Nothing
        (Right a, Just successes) -> Just (TooManySuccesses
                                              (a NE.<| successes) x)
        (Left e, Nothing)        -> Just (NoSuccesses (e :| otherFailures) x)
        (Left _, Just (_ :| [])) -> Nothing
        (Left _, Just successes) -> Just (TooManySuccesses successes x)
  where
    firstSuccess :: Either (JP.Index, NonEmpty err) (JP.Index, Value)
    firstSuccess =
        case NE.nonEmpty (f firstSubSchema x) of
            Nothing   -> Right (JP.Index 0, toJSON firstSubSchema)
            Just errs -> Left (JP.Index 0, errs)

    otherPerhapsFailures :: [(JP.Index, Value, [err])]
    otherPerhapsFailures =
        zipWith
            (\index schema -> (index, toJSON schema, f schema x))
            (JP.Index <$> [0..])
            otherSubSchemas

    otherSuccesses :: Maybe (NonEmpty (JP.Index, Value))
    otherSuccesses = NE.nonEmpty
                   $ mapMaybe (\(index,val,errs) ->
                                  case errs of
                                      [] -> Just (index,val)
                                      _  -> Nothing
                              ) otherPerhapsFailures

    otherFailures :: [(JP.Index, NonEmpty err)]
    otherFailures = mapMaybe (traverse NE.nonEmpty . mid) otherPerhapsFailures

    mid :: (a,b,c) -> (a,c)
    mid (a,_,c) = (a,c)

--------------------------------------------------
-- * not
--------------------------------------------------

newtype NotValidator schema
    = NotValidator { _unNotValidator :: schema }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (NotValidator schema) where
    parseJSON = withObject "NotValidator" $ \o ->
        NotValidator <$> o .: "not"

data NotValidatorInvalid
    = NotValidatorInvalid Value Value
    deriving (Eq, Show)

notVal
    :: ToJSON schema =>
       (schema -> Value -> [err])
    -> NotValidator schema
    -> Value
    -> Maybe NotValidatorInvalid
notVal f (NotValidator schema) x =
    case f schema x of
        [] -> Just (NotValidatorInvalid (toJSON schema) x)
        _  -> Nothing
