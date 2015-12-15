{-# LANGUAGE ScopedTypeVariables #-}

module Data.JsonSchema.Draft4.Objects
  ( module Data.JsonSchema.Draft4.Objects
  , module Data.JsonSchema.Draft4.Objects.Properties
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Hashable
import qualified Data.HashMap.Strict     as H
import qualified Data.Vector             as V

import           Data.JsonSchema.Core
import           Data.JsonSchema.Draft4.Objects.Properties
import           Data.JsonSchema.Helpers
import           Import

data DependencyFailure err = SchemaDependency err | PropertyDependency

maxProperties :: ValidatorConstructor err [FailureInfo]
maxProperties _ _ _ val _ = do
  n <- fromJSONInt val
  greaterThanZero n
  Just $ \x ->
    case x of
      Object o ->
        if H.size o > n
          then pure (FailureInfo val x mempty)
          else mempty
      _ -> mempty

minProperties :: ValidatorConstructor err [FailureInfo]
minProperties _ _ _ val _ = do
  n <- fromJSONInt val
  greaterThanZero n
  Just $ \x ->
    case x of
      Object o ->
        if H.size o < n
          then pure (FailureInfo val x mempty)
          else mempty
      _ -> mempty

required :: ValidatorConstructor err [FailureInfo]
required _ _ _ val@(Array vs) _ = do
  when (V.length vs == 0) Nothing
  ts <- traverse toTxt vs
  let a = vectorToMapSet ts
  when (H.size a /= V.length ts) Nothing
  Just $ \x ->
    case x of
      Object o ->
        if H.size (H.difference a o) > 0
          then pure (FailureInfo val x mempty)
          else mempty
      _ -> mempty
  where
    vectorToMapSet :: (Eq a, Hashable a) => Vector a -> HashMap a Bool
    vectorToMapSet vec = H.fromList . V.toList $ (\x -> (x, True)) <$> vec -- TODO: use a fold.
required _ _ _ _ _ = Nothing

-- http://json-schema.org/latest/json-schema-validation.html#anchor70
--
-- > This keyword's value MUST be an object.
-- > Each value of this object MUST be either an object or an array.
-- >
-- > If the value is an object, it MUST be a valid JSON Schema.
-- > This is called a schema dependency.
-- >
-- > If the value is an array, it MUST have at least one element.
-- > Each element MUST be a string, and elements in the array MUST be unique.
-- > This is called a property dependency.
dependencies :: ValidatorConstructor err [ValidationFailure (DependencyFailure err)]
dependencies spec g s val@(Object o) _ = do
  let vs = H.toList o
      schemaDeps = vs >>= toSchemaDep spec g
      propDeps = vs >>= toPropDep
  when (length schemaDeps + length propDeps /= length vs) Nothing
  Just $ \x ->
    case x of
      Object y ->
        let schemaFailures = join $ valSD y <$> schemaDeps
            propertyFailures = join $ valPD y <$> propDeps
        in schemaFailures <> propertyFailures
      _ -> mempty
  where
    toSchemaDep :: Spec a -> SchemaGraph -> (Text, Value) -> [(Text, Schema a)]
    toSchemaDep spc gr (t, Object ob) = pure (t, compile spc gr $ RawSchema (_rsURI s) ob)
    toSchemaDep _ _ _ = mempty

    toPropDep :: (Text, Value) -> [(Text, Vector Text)]
    toPropDep (t, Array a) =
      if V.length a <= 0
        then mempty
        else case traverse toTxt a of
          Nothing -> mempty
          Just ts ->
            if allUnique ts
              then pure (t, ts)
              else mempty
    toPropDep _ = mempty

    valSD :: HashMap Text Value -> (Text, Schema err) -> [ValidationFailure (DependencyFailure err)]
    valSD d (k, subSchema) =
      case H.lookup k d of
        Nothing -> mempty
        Just _  -> modifyFailureName SchemaDependency <$> validate subSchema (Object d)

    valPD :: HashMap Text Value -> (Text, Vector Text) -> [ValidationFailure (DependencyFailure err)]
    valPD d (k, ks) =
      case H.lookup k d of
        Nothing -> mempty
        Just _  ->
          case traverse (flip H.lookup d) ks of
            Nothing -> pure $ ValidationFailure PropertyDependency (FailureInfo val (Object d) mempty)
            Just _  -> mempty
dependencies _ _ _ _ _ = Nothing
