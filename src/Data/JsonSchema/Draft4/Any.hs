
module Data.JsonSchema.Draft4.Any where

import           Control.Monad
import qualified Data.HashMap.Strict       as H
import           Data.Maybe
import           Data.Scientific
import qualified Data.Vector               as V

import           Data.JsonSchema.Core
import           Data.JsonSchema.Helpers
import           Data.JsonSchema.Reference
import           Import

-- For GHCs before 7.10:
import           Prelude                   hiding (any)

-- | http://json-schema.org/latest/json-schema-validation.html#anchor76
--
--  > The value of this keyword MUST be an array.
--  > This array MUST have at least one element.
--  > Elements in the array MUST be unique.
--  >
--  > Elements in the array MAY be of any type, including null.
--
-- NOTE: We actually respect this, and don't build the validator
-- if any of the elements aren't unique.
enum :: ValidatorConstructor err [FailureInfo]
enum _ _ _ val@(Array vs) path = do
  when (V.null vs || not (allUniqueValues vs)) Nothing
  Just $ \x ->
    if V.elem x vs
      then mempty
      else pure (FailureInfo val x path)
enum _ _ _ _ _ = Nothing

typeValidator :: ValidatorConstructor err [FailureInfo]
typeValidator _ _ _ (String val) path = Just $ \x -> isJsonType path x (pure val)
typeValidator _ _ _ (Array vs) path = do
  ts <- traverse toTxt vs
  unless (allUnique ts) Nothing
  Just (\x -> isJsonType path x ts)
typeValidator _ _ _ _ _ = Nothing

isJsonType :: JSONPath -> Value -> Vector Text -> [FailureInfo]
isJsonType path x xs =
  case x of
    (Null)     -> f "null"    xs
    (Array _)  -> f "array"   xs
    (Bool _)   -> f "boolean" xs
    (Object _) -> f "object"  xs
    (String _) -> f "string"  xs
    (Number y) ->
      case toBoundedInteger y :: Maybe Int of
        Nothing -> f "number" xs
        Just _  -> if V.elem "integer" xs
                     then mempty
                     else f "number" xs
  where
    f :: Text -> Vector Text -> [FailureInfo]
    f t ts = if V.elem t ts
               then mempty
               else pure $ FailureInfo (Array (String <$> xs)) x path

allOf :: ValidatorConstructor err [ValidationFailure err]
allOf spec g s (Array vs) path = do
  os <- traverse toObj vs
  let subSchemas = compile spec g path . RawSchema (_rsURI s) <$> V.toList os
  Just $ \x -> join $ flip validate x <$> subSchemas
allOf _ _ _ _ _ = Nothing

anyOf :: ValidatorConstructor err [FailureInfo]
anyOf spec g s val@(Array vs) path = do
  os <- traverse toObj vs
  let subSchemas = compile spec g path . RawSchema (_rsURI s) <$> os
  Just $ \x ->
    if any null (flip validate x <$> subSchemas)
      then mempty
      else pure (FailureInfo val x path)
anyOf _ _ _ _ _ = Nothing

oneOf :: ValidatorConstructor err [FailureInfo]
oneOf spec g s val@(Array vs) path = do
  os <- traverse toObj $ V.toList vs
  let subSchemas = compile spec g path . RawSchema (_rsURI s) <$> os
  Just $ \x ->
    if (length . filter null $ flip validate x <$> subSchemas) == 1
      then mempty
      else pure (FailureInfo val x path)
oneOf _ _ _ _ _ = Nothing

notValidator :: ValidatorConstructor err [FailureInfo]
notValidator spec g s val@(Object o) path = do
  let subSchema = compile spec g path $ RawSchema (_rsURI s) o
  Just $ \x ->
    case validate subSchema x of
      [] -> pure (FailureInfo val x path)
      _  -> mempty
notValidator _ _ _ _ _ = Nothing

-- JSON Reference Draft Document:
--
--      http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03
ref :: ValidatorConstructor err [ValidationFailure err]
ref spec g s (String val) path = do
  let (mUri, mFragment) = resolveReference (_rsURI s) val
  r <- RawSchema mUri <$> getReference mUri
  let o = resolveFragment mFragment (_rsData r)
  return . validate . compile spec g path $ RawSchema (_rsURI r) o
  where
    getReference :: Maybe Text -> Maybe (HashMap Text Value)
    getReference Nothing  = Just . _rsData . _startingSchema $ g
    getReference (Just t) = H.lookup t (_cachedSchemas g)

ref _ _ _ _ _ = Nothing
