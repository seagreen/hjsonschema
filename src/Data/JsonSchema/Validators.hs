-- | This is generally meant to be an internal module. It's only
-- exposed in case you want to make your own 'Spec'. If you just
-- want to use JSON Schema Draft 4 use the preassembled
-- 'Data.JsonSchema.draft4' instead.

module Data.JsonSchema.Validators where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Fixed                (mod')
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as H
import           Data.JsonPointer
import           Data.JsonSchema.Core
import           Data.JsonSchema.Helpers
import           Data.JsonSchema.Reference
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Encoding
import           Data.Traversable
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import           Network.HTTP.Types.URI
import           Text.RegexPR

--------------------------------------------------
-- * Number Validators
--------------------------------------------------

multipleOf :: ValidatorGen
multipleOf _ _ _ (Number val) = do
  greaterThanZero val
  Just (\x ->
    case x of
      Number y ->
        if y `mod'` val /= 0
          then V.singleton $ tshow y <> " isn't a multiple of " <> tshow val
          else mempty
      _ -> mempty)
multipleOf _ _ _ _ = Nothing

maximumVal :: ValidatorGen
maximumVal _ _ s (Number val) =
  let f = case H.lookup "exclusiveMaximum" (_rsObject s) of
            Just (Bool a) -> if a then (>=) else (>)
            _             -> (>)
  in Just (\x ->
    case x of
      Number y ->
        if y `f` val
          then V.singleton $ tshow y <> " fails to validate against maximum " <> tshow val
          else mempty
      _ -> mempty)
maximumVal _ _ _ _ = Nothing

minimumVal :: ValidatorGen
minimumVal _ _ s (Number val) =
  let f = case H.lookup "exclusiveMinimum" (_rsObject s) of
            Just (Bool a) -> if a then (<=) else (<)
            _             -> (<)
  in Just (\x ->
    case x of
      Number y ->
        if y `f` val
          then V.singleton $ tshow y <> " fails to validate against minimum " <> tshow val
          else mempty
      _ -> mempty)
minimumVal _ _ _ _ = Nothing

--------------------------------------------------
-- * String Validators
--------------------------------------------------

maxLength :: ValidatorGen
maxLength _ _ _ v = do
  val <- fromJSONInt v
  greaterThanZero val
  Just (\x ->
    case x of
      String y ->
        if T.length y > val
          then V.singleton $ y <> " is greater than maxLength " <> tshow val
          else mempty
      _ -> mempty)

minLength :: ValidatorGen
minLength _ _ _ v = do
  val <- fromJSONInt v
  greaterThanZero val
  Just (\x ->
    case x of
      String y ->
        if T.length y < val
          then V.singleton $ y <> " is less than minLength " <> tshow val
          else mempty
      _ -> mempty)

pattern :: ValidatorGen
pattern _ _ _ (String val) =
  Just (\x ->
    case x of
      String t ->
        case matchRegexPR (T.unpack val) (T.unpack t) of
          Nothing -> V.singleton $ t <> " fails to validate against pattern " <> val
          Just _  -> mempty
      _ -> mempty)
pattern _ _ _ _ = Nothing

--------------------------------------------------
-- * Array Validators
--------------------------------------------------

-- | Also covers additionalItems.
items :: ValidatorGen
items spec g s (Object val) =
  let sub = compile spec g (RawSchema (_rsURI s) val)
  in Just (\x ->
    case x of
      Array ys -> ys >>= validate sub
      _        -> mempty)
items spec g s (Array vs) = do
  os <- traverse toObj vs
  let ss = compile spec g . RawSchema (_rsURI s) <$> os
  let addItems = do
        a <- H.lookup "additionalItems" (_rsObject s)
        additionalItems spec g s a
  Just (\x ->
    case x of
      (Array ys) ->
        let extras = V.drop (V.length os) ys
        in join (V.zipWith validate ss ys) <> runMaybeVal addItems (Array extras)
      _ -> mempty)
items _ _ _ _ = Nothing

-- | Not included directly in the 'draft4' spec hashmap because it always
-- validates data unless 'items' is also present. This is because 'items'
-- defaults to {}.
additionalItems :: ValidatorGen
additionalItems _ _ _ (Bool val) =
  Just (\x ->
    case x of
      Array ys ->
        if not val && V.length ys > 0
          then V.singleton ("Val error against additionalItems false for: " <> tshow x)
          else mempty
      _ -> mempty)
additionalItems spec g s (Object val) =
  let sub = compile spec g (RawSchema (_rsURI s) val)
  in Just (\x ->
    case x of
      Array ys -> ys >>= validate sub
      _        -> mempty)
additionalItems _ _ _ _ = Nothing

maxItems :: ValidatorGen
maxItems _ _ _ v = do
  val <- fromJSONInt v
  greaterThanZero val
  Just (\x ->
    case x of
      Array ys ->
        if V.length ys > val
          then V.singleton $ tshow ys <> " has more items than maxItems " <> tshow val
          else mempty
      _ -> mempty)

minItems :: ValidatorGen
minItems _ _ _ v = do
  val <- fromJSONInt v
  greaterThanZero val
  Just (\x ->
    case x of
      Array ys ->
        if V.length ys < val
          then V.singleton $ tshow ys <> " has fewer items than minItems " <> tshow val
          else mempty
      _ -> mempty)

uniqueItems :: ValidatorGen
uniqueItems _ _ _ (Bool val) = do
  unless val Nothing
  Just (\x ->
    case x of
      (Array ys) -> if allUnique ys
        then mempty
        else V.singleton ("Val error against uniqueItems " <> tshow val <> " for: " <> tshow x)
      _ -> mempty)
uniqueItems _ _ _ _ = Nothing

--------------------------------------------------
-- * Object Validators
--------------------------------------------------

maxProperties :: ValidatorGen
maxProperties _ _ _ v = do
  val <- fromJSONInt v
  greaterThanZero val
  Just (\x ->
    case x of
      Object o ->
        if H.size o > val
          then V.singleton $ tshow o <> " has more members than maxProperties " <> tshow val
          else mempty
      _ -> mempty)

minProperties :: ValidatorGen
minProperties _ _ _ v = do
  val <- fromJSONInt v
  greaterThanZero val
  Just (\x ->
    case x of
      Object o ->
        if H.size o < val
          then V.singleton $ tshow o <> " has fewer members than minProperties " <> tshow val
          else mempty
      _ -> mempty)

required :: ValidatorGen
required _ _ _ (Array vs) = do
  when (V.length vs == 0) Nothing
  ts <- traverse toTxt vs
  let a = vectorToMapSet ts
  when (H.size a /= V.length ts) Nothing
  Just (\x ->
    case x of
      Object o ->
        if H.size (H.difference a o) > 0
          then V.singleton $ "the keys of " <> tshow o <>
            " don't contain all the required elements " <> tshow vs
          else mempty
      _ -> mempty)
  where
    vectorToMapSet :: (Eq a, Hashable a) => Vector a -> HashMap a Bool
    vectorToMapSet vec = vectorToHm $ (\x -> (x, True)) <$> vec
required _ _ _ _ = Nothing

-- TODO: Fix up the properties validators. They're all a huge mess, but at least
-- they work.
--
-- In order of what's tried: properties, patternProperties, additionalProperties
properties :: ValidatorGen
properties spec g s v = do
  let mProps = propertiesMatches spec g s v
  let mPatProp = do
                  aV <- H.lookup "patternProperties" (_rsObject s)
                  patternPropertiesMatches spec g s aV
  let mAdd = do
              aVal <- H.lookup "additionalProperties" (_rsObject s)
              runAdditionalProperties spec g s aVal
  when (isNothing mProps && isNothing mPatProp && isNothing mAdd) Nothing
  Just (\x ->
    case x of
      Object y -> -- Got myself into a mess here.
        let (e1s, remaining) = runMaybeVal' mProps (Object y)
            (_, remaining') = runMaybeVal' mPatProp remaining
            (e2s, _) = runMaybeVal' mPatProp (Object y)
        in e1s <> e2s <> runMaybeVal mAdd remaining'
      _ -> mempty)

patternProperties :: ValidatorGen
patternProperties spec g s v = do
  when (H.member "properties" (_rsObject s)) Nothing
  let mPatProp = patternPropertiesMatches spec g s v
  -- TODO: checking additionalProperties as well doesn't help with tests
  let mAdd = do
              aVal <- H.lookup "additionalProperties" (_rsObject s)
              runAdditionalProperties spec g s aVal
  when (isNothing mPatProp && isNothing mAdd) Nothing
  Just (\x ->
    case x of
      Object y ->
        let (e2s, remaining') = runMaybeVal' mPatProp (Object y)
        in e2s <> runMaybeVal mAdd remaining'
      _ -> mempty)

-- | An implementation of the "additionalProperties" keyword that never
-- disables itself. Not included directly in the 'draft4' spec hashmap.
runAdditionalProperties :: ValidatorGen
runAdditionalProperties _ _ _ (Bool val) =
  Just (\x ->
    case x of
      Object y ->
        if not val && H.size y > 0
          then V.singleton ("Val error against additionalProperties false for: " <> tshow x)
          else mempty
      _ -> mempty)
runAdditionalProperties spec g s (Object val) =
  let sub = compile spec g (RawSchema (_rsURI s) val)
  in Just (\x ->
    case x of
      Object y -> vectorOfElems y >>= validate sub
      _        -> mempty)
runAdditionalProperties _ _ _ _ = Nothing

additionalProperties :: ValidatorGen
additionalProperties spec g s v = do
  when (H.member "properties" (_rsObject s)) Nothing
  when (H.member "patternProperties" (_rsObject s)) Nothing
  runAdditionalProperties spec g s v

-- http://json-schema.org/latest/json-schema-validation.html#anchor70
--
--  This keyword's value MUST be an object.
--  Each value of this object MUST be either an object or an array.
--
-- If the value is an object, it MUST be a valid JSON Schema.
-- This is called a schema dependency.
--
-- If the value is an array, it MUST have at least one element.
-- Each element MUST be a string, and elements in the array MUST be unique.
-- This is called a property dependency.
dependencies :: ValidatorGen
dependencies spec g s (Object val) = do
  let vs = hmToVector val
  let schemaDeps = vs >>= toSchemaDep spec g
  let propDeps = vs >>= toPropDep
  when (V.length schemaDeps + V.length propDeps /= V.length vs) Nothing
  Just (\x ->
    case x of
      Object y -> join $ (valSD <$> schemaDeps <*> pure y)
                         <> (valPD <$> propDeps <*> pure y)
      _        -> mempty)
  where
    toSchemaDep :: Spec -> Graph -> (Text, Value) -> Vector (Text, Schema)
    toSchemaDep spc gr (t, Object o) =
      V.singleton (t, compile spc gr $ RawSchema (_rsURI s) o)
    toSchemaDep _ _ _ = mempty

    toPropDep :: (Text, Value) -> Vector (Text, Vector Text)
    toPropDep (t, Array a) =
      if V.length a <= 0
        then mempty
        else case traverse toTxt a of
          Nothing -> mempty
          Just ts ->
            if allUnique ts
              then V.singleton (t, ts)
              else mempty
    toPropDep _ = mempty

    valSD :: (Text, Schema) -> HashMap Text Value -> Vector ValErr
    valSD (t, sub) d =
      case H.lookup t d of
        Nothing -> mempty
        Just _  -> validate sub (Object d)

    valPD :: (Text, Vector Text) -> HashMap Text Value -> Vector ValErr
    valPD (t, ts) d =
      case H.lookup t d of
        Nothing -> mempty
        Just _  ->
          case traverse ($ d) (H.lookup <$> ts) of
            Nothing -> V.singleton
              ("Val error against property dependency with the key "
               <> t <> " and the value " <> tshow ts <> " for: " <> tshow d)
            Just _  -> mempty

dependencies _ _ _ _ = Nothing

--------------------------------------------------
-- * Any Validators
--------------------------------------------------

enum :: ValidatorGen
enum _ _ _ (Array vs) = do
  unless (V.length vs > 0 && allUnique vs) Nothing
  Just (\x ->
    if V.elem x vs
      then mempty
      else V.singleton $ tshow x <> " is not an element of enum " <> tshow vs)
enum _ _ _ _ = Nothing

typeVal :: ValidatorGen
typeVal _ _ _ (String val) = Just (\x -> isJsonType x (V.singleton val))
typeVal _ _ _ (Array vs) = do
  ts <- traverse toTxt vs
  unless (allUnique ts) Nothing
  Just (`isJsonType` ts)
typeVal _ _ _ _ = Nothing

allOf :: ValidatorGen
allOf spec g s (Array vs) = do
  os <- traverse toObj vs
  let ss = compile spec g . RawSchema (_rsURI s) <$> os
  Just (\x -> join $ validate <$> ss <*> pure x)
allOf _ _ _ _ = Nothing

anyOf :: ValidatorGen
anyOf spec g s (Array vs) = do
  os <- traverse toObj vs
  let ss = compile spec g . RawSchema (_rsURI s) <$> os
  Just (\x ->
    if V.elem V.empty (validate <$> ss <*> pure x)
      then mempty
      else V.singleton ("Val error against anyOf " <> tshow vs <> " for: " <> tshow x))
anyOf _ _ _ _ = Nothing

oneOf :: ValidatorGen
oneOf spec g s (Array vs) = do
  os <- traverse toObj vs
  let ss = compile spec g . RawSchema (_rsURI s) <$> os
  Just (\x ->
    if count V.empty (validate <$> ss <*> pure x) == 1
      then mempty
      else V.singleton ("Val error against oneOf " <> tshow vs <> " for: " <> tshow x))
oneOf _ _ _ _ = Nothing

notValidator :: ValidatorGen
notValidator spec g s (Object val) = do
  let sub = compile spec g (RawSchema (_rsURI s) val)
  Just (\x ->
    if V.null $ validate sub x
      then V.singleton ("Val error against not validator " <> tshow val <> " for: " <> tshow x)
      else mempty)
notValidator _ _ _ _ = Nothing

-- http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03
--
-- TODO: Any members other than "$ref" in a JSON Reference object SHALL be
-- ignored.
ref :: ValidatorGen
ref spec g s (String val) = do
  (reference, pointer) <- refAndPointer (_rsURI s `combineIdAndRef` val)
  r <- RawSchema reference <$> H.lookup reference g
  let urlDecoded = decodeUtf8 . urlDecode True . encodeUtf8 $ pointer
  p <- eitherToMaybe $ jsonPointer urlDecoded
  case resolvePointer p (Object $ _rsObject r) of
    Right (Object o) ->
      Just $ validate $ compile spec g $ RawSchema (_rsURI r) o
    _                -> Nothing
ref _ _ _ _ = Nothing

noVal :: ValidatorGen
noVal _ _ _ _ = Just (const V.empty)
