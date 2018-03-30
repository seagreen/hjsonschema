module JSONSchema.Validator.Draft4.Any where

import           Import hiding ((<>))

import           Data.Aeson.TH (constructorTagModifier)
import           Data.Char (toLower)
import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Scientific as SCI
import           Data.Semigroup
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text.Encoding.Error (UnicodeException)
import qualified JSONPointer as JP
import           Network.HTTP.Types.URI (urlDecode)

import           JSONSchema.Validator.Reference (BaseURI(..), Scope(..),
                                                 URIAndFragment,
                                                 resolveReference)
import qualified JSONSchema.Validator.Utils as UT

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
    | RefPointerResolution JSONPointerError
    | RefLoop              Text VisitedSchemas URIAndFragment
    | RefInvalid           Text Value (NonEmpty err)
      -- ^ 'Text' is the URI and 'Value' is the linked schema.
    deriving (Eq, Show)

newtype VisitedSchemas
    = VisitedSchemas { _unVisited :: [URIAndFragment] }
    deriving (Eq, Show, Semigroup, Monoid)

refVal
    :: forall err schema. (FromJSON schema, ToJSON schema)
    => (Text -> Maybe schema)
        -- ^ Look up a schema.
    -> (BaseURI -> schema -> BaseURI)
        -- ^ Update scope (needed after moving deeper into nested schemas).
    -> (VisitedSchemas -> Scope schema -> schema -> Value -> [err])
        -- ^ Validate data.
    -> VisitedSchemas
    -> Scope schema
    -> Ref
    -> Value
    -> Maybe (RefInvalid err)
refVal getRef updateScope val visited scope (Ref reference) x
    | (mURI, mFragment) `elem` _unVisited visited =
        Just (RefLoop reference visited (mURI, mFragment))
    | otherwise = leftToMaybe $ do

        -- Get the referenced document

        (newScope, doc) <- first RefResolution
                         $ getDocument getRef updateScope scope mURI reference

        -- Get the correct subschema within that document.

        res <- case mFragment of
                   Nothing       -> Right (newScope, doc)
                   Just fragment -> first RefPointerResolution
                                  $ resolveFragment updateScope newScope fragment
        let (finalScope, schema) = res

        -- Check if that schema is valid.

        let newVisited = VisitedSchemas [(_documentURI newScope, mFragment)]
                      <> visited
            failures = val newVisited finalScope schema x
        first (RefInvalid reference (toJSON schema))
            . maybeToLeft ()
            $ NE.nonEmpty failures
  where
    mURI      :: Maybe Text
    mFragment :: Maybe Text
    (mURI, mFragment) = resolveReference (_currentBaseURI scope) reference

getDocument
    :: forall schema. (Text -> Maybe schema)
    -> (BaseURI -> schema -> BaseURI)
    -> Scope schema
    -> Maybe Text
    -> Text
    -> Either Text (Scope schema, schema)
    -- ^ 'Left' is the URI of the document we failed to resolve.
getDocument getRef updateScope scope mURI reference =
    case mURI <* fst (resolveReference (BaseURI Nothing) reference) of
        Nothing  -> Right topOfThisDoc
        Just uri ->
            case getRef uri of
                Nothing -> Left uri
                Just s  -> Right ( Scope s mURI (updateScope (BaseURI mURI) s)
                                 , s
                                 )
  where
    topOfThisDoc :: (Scope schema, schema)
    topOfThisDoc =
        ( scope { _currentBaseURI =
                    updateScope (BaseURI (_documentURI scope))
                                 (_topLevelDocument scope)
                }
        , _topLevelDocument scope
        )

data JSONPointerError
    = URLDecodingError       UnicodeException
        -- ^ Aspirationally internal.
    | FormatError            JP.FormatError
    | ResolutionError        JP.ResolutionError
    | SubschemaDecodingError Text
        -- ^ Aspirationally internal.
    deriving (Eq, Show)

resolveFragment
    :: (FromJSON schema, ToJSON schema)
    => (BaseURI -> schema -> BaseURI)
    -> Scope schema
    -> Text
    -> Either JSONPointerError (Scope schema, schema)
resolveFragment updateScope scope fragment = do
    urlDecoded <- first URLDecodingError
                . decodeUtf8'
                . urlDecode True
                . encodeUtf8
                $ fragment
    JP.Pointer tokens <- first FormatError (JP.unescape urlDecoded)
    let acc = (toJSON (_topLevelDocument scope), _currentBaseURI scope)
    (schemaVal, base) <- foldM go acc tokens
    schema <- first SubschemaDecodingError (fromJSONEither schemaVal)
    pure (scope { _currentBaseURI = base }, schema)
  where
    -- We have to step through the document JSON Pointer token
    -- by JSON Pointer token so that we can update the scope
    -- based on each @"id"@ we encounter.
    --
    -- TODO: Do we need specialized code to skip @"id"@s such
    -- as property keys that aren't meant to change scope?
    -- Perhaps this should be added to the language agnostic
    -- test suite as well.
    --
    -- In the meantime 'newBaseURIFromFragment' drops all keys
    -- from JSON objects except "id", which at least prevents
    -- SubschemaDecodingError in a situation where one of the
    -- values we step through isn't a valid schema.
    go :: (Value, BaseURI)
       -> JP.Token
       -> Either JSONPointerError (Value, BaseURI)
    go (lastVal, baseURI) tok = do
        v <- first ResolutionError (JP.resolveToken tok lastVal)
        newBase <- newBaseURIFromFragment updateScope baseURI v
        Right (v, newBase)

-- | Update the 'BaseURI' (the store of the current "id" value)
-- after resolving one token of a JSON Pointer and stepping into
-- a new 'Value'.
newBaseURIFromFragment
    :: FromJSON schema
    => (BaseURI -> schema -> BaseURI)
    -> BaseURI
    -> Value
    -> Either JSONPointerError BaseURI
newBaseURIFromFragment updateScope baseURI v =
  case v of
    Object hm -> do
      let hmWithOnlyId = case HM.lookup idKey hm of
                           Nothing    -> mempty
                           Just idVal -> HM.singleton idKey idVal
      schema <- first SubschemaDecodingError (fromJSONEither (Object hmWithOnlyId))
      Right (updateScope baseURI schema)
    _ -> Right baseURI
  where
    idKey :: Text
    idKey = "id"

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
        toUnique = fmap UT._unOrdValue
                 . Set.toList
                 . Set.fromList
                 . fmap UT.OrdValue

data EnumInvalid
    = EnumInvalid EnumValidator Value
    deriving (Eq, Show)

enumVal :: EnumValidator -> Value -> Maybe EnumInvalid
enumVal a@(EnumValidator vs) x
    | not (UT.allUniqueValues vs) = Nothing
    | x `elem` vs                 = Nothing
    | otherwise                   = Just $ EnumInvalid a x

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
    = TypeValidatorString SchemaType
    | TypeValidatorArray  (Set SchemaType)
    deriving (Eq, Show)

instance Semigroup TypeValidator where
    (<>) x y
        | isEmpty x = x
        | isEmpty y = y
        | x == y    = x
        | otherwise = TypeValidatorArray (setFromTypeValidator x
                                          `Set.union`
                                          setFromTypeValidator y)
      where
        isEmpty :: TypeValidator -> Bool
        isEmpty (TypeValidatorString _) = False
        isEmpty (TypeValidatorArray ts) = Set.null ts

    stimes = stimesIdempotent

instance FromJSON TypeValidator where
    parseJSON v = fmap TypeValidatorString (parseJSON v)
              <|> fmap TypeValidatorArray (parseJSON v)

instance ToJSON TypeValidator where
    toJSON (TypeValidatorString t) = toJSON t
    toJSON (TypeValidatorArray ts) = toJSON ts

instance Arbitrary TypeValidator where
    arbitrary = oneof [ TypeValidatorString <$> arbitrary
                      , TypeValidatorArray <$> arbitrary
                      ]

data SchemaType
    = SchemaObject
    | SchemaArray
    | SchemaString
    | SchemaNumber
    | SchemaInteger
    | SchemaBoolean
    | SchemaNull
    deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance FromJSON SchemaType where
    parseJSON = genericParseJSON
                    defaultOptions
                    { constructorTagModifier = fmap toLower . drop 6 }

instance ToJSON SchemaType where
    toJSON = genericToJSON
                 defaultOptions
                 { constructorTagModifier = fmap toLower . drop 6 }

instance Arbitrary SchemaType where
    arbitrary = arbitraryBoundedEnum

data TypeValidatorInvalid
    = TypeValidatorInvalid TypeValidator Value
    deriving (Eq, Show)

typeVal :: TypeContext -> Value -> Maybe TypeValidatorInvalid
typeVal (TypeContext tv) x
    | Set.null matches = Just (TypeValidatorInvalid tv x)
    | otherwise        = Nothing
  where
    -- There can be more than one match because a 'Value' can be both a
    -- @"number"@ and an @"integer"@.
    matches :: Set SchemaType
    matches = Set.intersection okTypes (setFromTypeValidator tv)

    okTypes :: Set SchemaType
    okTypes =
        case x of
            Null       -> Set.singleton SchemaNull
            (Array _)  -> Set.singleton SchemaArray
            (Bool _)   -> Set.singleton SchemaBoolean
            (Object _) -> Set.singleton SchemaObject
            (String _) -> Set.singleton SchemaString
            (Number y) ->
                if SCI.isInteger y
                    then Set.fromList [SchemaNumber, SchemaInteger]
                    else Set.singleton SchemaNumber

-- | Internal.
setFromTypeValidator :: TypeValidator -> Set SchemaType
setFromTypeValidator (TypeValidatorString t) = Set.singleton t
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
    | any (null . snd) perhapsFailures = Nothing
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
