
module Data.JsonSchema.Draft4.Arrays where

import           Control.Monad
import qualified Data.HashMap.Strict     as H
import qualified Data.Vector             as V

import           Data.JsonSchema.Core
import           Data.JsonSchema.Helpers
import           Import

data ItemsFailure err
  = Items err
  | AdditionalItemsBool
  | AdditionalItemsObject err

data AdditionalItemsFailure err
  = AdditionalBool
  | AdditionalObject err

-- | A combination of items and additionalItems.
items :: ValidatorConstructor err [ValidationFailure (ItemsFailure err)]
items spec g s (Object o) path =
  let subSchema index = compile spec g (path ++ [JSONPathIndex index]) $ RawSchema (_rsURI s) o
  in Just $ \x ->
    case x of
      Array ys -> zip [0..] (V.toList ys) >>= \(index, val) -> fmap (modifyFailureName Items) $ validate (subSchema index) val
      _        -> mempty
items spec g s (Array vs) path = do
  os <- traverse toObj vs
  let subSchemas = zip [0..] (V.toList os) >>= \(index, val) -> pure $ compile spec g (path ++ [JSONPathIndex index]) (RawSchema (_rsURI s) val)
      mAdditionalItems = H.lookup "additionalItems" (_rsData s) >>= \is -> additionalItems spec g s is path
  Just $ \x ->
    case x of
      Array ys ->
        let extras = V.drop (V.length os) ys
            itemFailures = join $ fmap (modifyFailureName Items) <$> zipWith validate subSchemas (V.toList ys)
            additionalItemFailures = runMaybeVal mAdditionalItems (Array extras)
        in itemFailures <> fmap (modifyFailureName f) additionalItemFailures
      _ -> mempty
  where
    f :: AdditionalItemsFailure err -> ItemsFailure err
    f AdditionalBool         = AdditionalItemsBool
    f (AdditionalObject err) = AdditionalItemsObject err
items _ _ _ _ _ = Nothing

-- | Not included directly in the 'draft4' spec hashmap because it always
-- validates data unless 'items' is also present. This is because 'items'
-- defaults to {}.
additionalItems :: ValidatorConstructor err [ValidationFailure (AdditionalItemsFailure err)]
additionalItems _ _ _ val@(Bool v) path =
  Just $ \x ->
    case x of
      Array ys ->
        if not v && V.length ys > 0
          then pure $ ValidationFailure AdditionalBool (FailureInfo val x path)
          else mempty
      _ -> mempty
additionalItems spec g s (Object o) path =
  let subSchema index = compile spec g (path ++ [JSONPathIndex index]) (RawSchema (_rsURI s) o)
  in Just $ \x ->
    case x of
      Array ys -> zip [0..] (V.toList ys) >>= \(index, val) -> fmap (modifyFailureName AdditionalObject) $ validate (subSchema index) val
      _        -> mempty
additionalItems _ _ _ _ _ = Nothing

maxItems :: ValidatorConstructor err [FailureInfo]
maxItems _ _ _ val path = do
  n <- fromJSONInt val
  greaterThanZero n
  Just $ \x ->
    case x of
      Array ys ->
        if V.length ys > n
          then pure (FailureInfo val x path)
          else mempty
      _ -> mempty

minItems :: ValidatorConstructor err [FailureInfo]
minItems _ _ _ val path = do
  n <- fromJSONInt val
  greaterThanZero n
  Just $ \x ->
    case x of
      Array ys ->
        if V.length ys < n
          then pure (FailureInfo val x path)
          else mempty
      _ -> mempty

uniqueItems :: ValidatorConstructor err [FailureInfo]
uniqueItems _ _ _ val@(Bool v) path = do
  unless v Nothing
  Just $ \x ->
    case x of
      (Array ys) -> if allUniqueValues ys
        then mempty
        else pure (FailureInfo val x path)
      _ -> mempty
uniqueItems _ _ _ _ _ = Nothing
