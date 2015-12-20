{-# LANGUAGE ScopedTypeVariables #-}

module Data.JsonSchema.Draft4.Objects.Properties where

import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict     as H
import           Data.Maybe
import qualified Data.Text               as T
import           Text.RegexPR

import           Data.JsonSchema.Core
import           Data.JsonSchema.Helpers
import           Import

data PropertiesFailure err
  = Properties err
  | PropPattern err
  | PropAdditional (AdditionalPropertiesFailure err)

data PatternPropertiesFailure err
  = PatternProperties err
  | PatternAdditional (AdditionalPropertiesFailure err)

data AdditionalPropertiesFailure err
  = AdditionalPropertiesBool
  | AdditionalPropertiesObject err

-- | In order of what's tried: properties, patternProperties, additionalProperties
properties :: forall err. ValidatorConstructor err [ValidationFailure (PropertiesFailure err)]
properties spec g s val path = do
  let addProps = H.lookup "additionalProperties" (_rsData s)
  let patProps = H.lookup "patternProperties" (_rsData s)
  let mProps   = propertiesUnmatched val path
      mPatProp = patProps >>= \props -> patternUnmatched spec g s props path
      mAddProp = addProps >>= \props -> runAdditionalProperties spec g s props path

  when (isNothing mProps && isNothing mPatProp && isNothing mAddProp) Nothing
  Just $ \x ->
    case x of
      Object y ->
        let (propFailures, remaining) = runMaybeVal' mProps (Object y)
            patternFailures           = fst $ runMaybeVal' mPatProp (Object y)
            remaining'                = snd $ runMaybeVal' mPatProp remaining
            additionalFailures        = runMaybeVal mAddProp remaining'
        in fmap (modifyFailureName Properties) propFailures
             <> fmap (modifyFailureName PropPattern) patternFailures
             <> fmap (modifyFailureName PropAdditional) additionalFailures
      _ -> mempty
  where
    propertiesUnmatched :: Value -> JSONPath -> Maybe (Value -> ([ValidationFailure err], Value))
    propertiesUnmatched (Object o) path = do
      os <- traverse toObj o
      let matchedSchemas = H.mapWithKey (\k v -> compile spec g (path ++ [JSONPathKey k]) (RawSchema (_rsURI s) v)) os
      Just (\x ->
        case x of
          Object y ->
            let rawFailures = H.intersectionWith validate matchedSchemas y
                failures = join (H.elems rawFailures)
                leftovers = Object (H.difference y matchedSchemas)
            in (failures, leftovers)
          z -> (mempty, z))
    propertiesUnmatched _ _ = Nothing

patternProperties :: ValidatorConstructor err [ValidationFailure (PatternPropertiesFailure err)]
patternProperties spec g s val path = do
  when (H.member "properties" (_rsData s)) Nothing
  let addProps = H.lookup "additionalProperties" (_rsData s)
  let mPatternProps = patternUnmatched spec g s val path
  let mAdditionalProps = addProps >>= \props -> runAdditionalProperties spec g s props path
  when (isNothing mPatternProps && isNothing mAdditionalProps) Nothing
  Just $ \x ->
    case x of
      Object y ->
        let (ppFailures, remaining') = runMaybeVal' mPatternProps (Object y)
            addFailures              = runMaybeVal mAdditionalProps remaining'
        in fmap (modifyFailureName PatternProperties) ppFailures <> fmap (modifyFailureName PatternAdditional) addFailures
      _ -> mempty

patternUnmatched
  :: Spec err
  -> SchemaGraph
  -> RawSchema
  -> Value
  -> JSONPath
  -> Maybe (Value -> ([ValidationFailure err], Value))
patternUnmatched spec g s (Object val) path = do
  os <- traverse toObj val
  let subSchemas = H.mapWithKey (\k v -> compile spec g (path ++ [JSONPathKey k]) (RawSchema (_rsURI s) v)) os
  Just (\x ->
    case x of
      Object y -> let ms = H.foldlWithKey' (matches subSchemas) mempty y
                  in (H.foldl' runVals mempty ms, Object (leftovers ms))
      _        -> (mempty, x))
  where
    matches
      :: HashMap Text (Schema a)
      -> HashMap Text (Value, [Schema a])
      -> Text
      -> Value
      -> HashMap Text (Value, [Schema a])
    matches subSchemas acc k v = H.insert k (v, H.foldlWithKey' (match k) mempty subSchemas) acc

    match
      :: Text
      -> [Schema a]
      -> Text
      -> Schema a
      -> [Schema a]
    match k acc r subSchema =
      case matchRegexPR (T.unpack r) (T.unpack k) of
        Nothing -> acc
        Just _  -> pure subSchema <> acc

    runVals
      :: [ValidationFailure err]
      -> (Value, [Schema err])
      -> [ValidationFailure err]
    runVals acc (v,subSchema) = (subSchema >>= flip validate v) <> acc

    leftovers :: HashMap Text (Value, [Schema a]) -> HashMap Text Value
    leftovers possiblyMatched = fst <$> H.filter (null . snd) possiblyMatched
patternUnmatched _ _ _ _ _ = Nothing

additionalProperties :: ValidatorConstructor err [ValidationFailure (AdditionalPropertiesFailure err)]
additionalProperties spec g s val path = do
  when (H.member "properties" (_rsData s)) Nothing
  when (H.member "patternProperties" (_rsData s)) Nothing
  runAdditionalProperties spec g s val path

-- | An additionalProperties validator than never disables itself.
--
-- Not meant to be used standalone, but useful inside of the properties
-- and patternProperties validators.
runAdditionalProperties :: ValidatorConstructor err [ValidationFailure (AdditionalPropertiesFailure err)]
runAdditionalProperties _ _ _ val@(Bool v) path =
  Just $ \x ->
    case x of
      Object y ->
        if not v && H.size y > 0
          then pure $ ValidationFailure AdditionalPropertiesBool (FailureInfo val x path)
          else mempty
      _ -> mempty
runAdditionalProperties spec g s (Object o) path =
  let sub p = compile spec g (path ++ [JSONPathKey p]) $ RawSchema (_rsURI s) o
  in Just $ \x ->
    case x of
      Object y -> H.toList y >>= \(k, v) -> fmap (modifyFailureName AdditionalPropertiesObject) $ validate (sub k) v
      _        -> mempty
runAdditionalProperties _ _ _ _ _ = Nothing
