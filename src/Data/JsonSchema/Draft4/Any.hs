
module Data.JsonSchema.Draft4.Any where

import           Control.Monad
import qualified Data.HashMap.Strict       as H
import           Data.JsonPointer
import           Data.Scientific
import           Data.Text.Encoding
import qualified Data.Vector               as V
import           Network.HTTP.Types.URI

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
enum _ _ _ val@(Array vs) = do
  when (V.null vs || not (allUniqueValues vs)) Nothing
  Just $ \x ->
    if V.elem x vs
      then mempty
      else pure (FailureInfo val x)
enum _ _ _ _ = Nothing

typeValidator :: ValidatorConstructor err [FailureInfo]
typeValidator _ _ _ (String val) = Just $ \x -> isJsonType x (pure val)
typeValidator _ _ _ (Array vs) = do
  ts <- traverse toTxt vs
  unless (allUnique ts) Nothing
  Just (`isJsonType` ts)
typeValidator _ _ _ _ = Nothing

isJsonType :: Value -> Vector Text -> [FailureInfo]
isJsonType x xs =
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
               else pure $ FailureInfo (Array (String <$> xs)) x

allOf :: ValidatorConstructor err [ValidationFailure err]
allOf spec g s (Array vs) = do
  os <- traverse toObj vs
  let subSchemas = compile spec g . RawSchema (_rsURI s) <$> V.toList os
  Just $ \x -> join $ flip validate x <$> subSchemas
allOf _ _ _ _ = Nothing

anyOf :: ValidatorConstructor err [FailureInfo]
anyOf spec g s val@(Array vs) = do
  os <- traverse toObj vs
  let subSchemas = compile spec g . RawSchema (_rsURI s) <$> os
  Just $ \x ->
    if any null (flip validate x <$> subSchemas)
      then mempty
      else pure (FailureInfo val x)
anyOf _ _ _ _ = Nothing

oneOf :: ValidatorConstructor err [FailureInfo]
oneOf spec g s val@(Array vs) = do
  os <- traverse toObj $ V.toList vs
  let subSchemas = compile spec g . RawSchema (_rsURI s) <$> os
  Just $ \x ->
    if (length . filter null $ flip validate x <$> subSchemas) == 1
      then mempty
      else pure (FailureInfo val x)
oneOf _ _ _ _ = Nothing

notValidator :: ValidatorConstructor err [FailureInfo]
notValidator spec g s val@(Object o) = do
  let sub = compile spec g (RawSchema (_rsURI s) o)
  Just $ \x ->
    case validate sub x of
      [] -> pure (FailureInfo val x)
      _  -> mempty
notValidator _ _ _ _ = Nothing

-- http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03
--
-- TODO: Any members other than "$ref" in a JSON Reference object SHALL be
-- ignored.
ref :: ValidatorConstructor err [ValidationFailure err]
ref spec g s (String val) = do
  (reference, pointer) <- refAndPointer (_rsURI s `combineIdAndRef` val)
  r <- RawSchema reference <$> H.lookup reference g
  let urlDecoded = decodeUtf8 . urlDecode True . encodeUtf8 $ pointer
  p <- eitherToMaybe $ jsonPointer urlDecoded
  case resolvePointer p (Object $ _rsObject r) of
    Right (Object o) ->
      let compiled = compile spec g $ RawSchema (_rsURI r) o
      in Just $ validate compiled
    _ -> Nothing
ref _ _ _ _ = Nothing
