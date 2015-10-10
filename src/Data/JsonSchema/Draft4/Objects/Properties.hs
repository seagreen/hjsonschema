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
properties spec g s val = do
  let mProps   = propertiesUnmatched val
      mPatProp = patternUnmatched spec g s =<< H.lookup "patternProperties" (_rsObject s)
      mAddProp = runAdditionalProperties spec g s =<< H.lookup "additionalProperties" (_rsObject s)
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
    propertiesUnmatched :: Value -> Maybe (Value -> ([ValidationFailure err], Value))
    propertiesUnmatched (Object o) = do
      os <- traverse toObj o
      let matchedSchemas = compile spec g . RawSchema (_rsURI s) <$> os
      Just (\x ->
        case x of
          Object y ->
            let rawFailures = H.intersectionWith validate matchedSchemas y
                failures = join (H.elems rawFailures)
                leftovers = Object (H.difference y matchedSchemas)
            in (failures, leftovers)
          z -> (mempty, z))
    propertiesUnmatched _ = Nothing

patternProperties :: ValidatorConstructor err [ValidationFailure (PatternPropertiesFailure err)]
patternProperties spec g s val = do
  when (H.member "properties" (_rsObject s)) Nothing
  let mPatternProps = patternUnmatched spec g s val
  -- TODO: checking additionalProperties as well doesn't help with tests.
  -- Make sure we're doing the correct thing, then get tests for this
  -- merged into the test suite.
  let mAdditionalProps = runAdditionalProperties spec g s =<< H.lookup "additionalProperties" (_rsObject s)
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
  -> Graph
  -> RawSchema
  -> Value
  -> Maybe (Value -> ([ValidationFailure err], Value))
patternUnmatched spec g s (Object val) = do
  os <- traverse toObj val
  let subSchemas = compile spec g . RawSchema (_rsURI s) <$> os
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
patternUnmatched _ _ _ _ = Nothing

additionalProperties :: ValidatorConstructor err [ValidationFailure (AdditionalPropertiesFailure err)]
additionalProperties spec g s val = do
  when (H.member "properties" (_rsObject s)) Nothing
  when (H.member "patternProperties" (_rsObject s)) Nothing
  runAdditionalProperties spec g s val

-- | An additionalProperties validator than never disables itself.
--
-- Not meant to be used standalone, but useful inside of the properties
-- and patternProperties validators.
runAdditionalProperties :: ValidatorConstructor err [ValidationFailure (AdditionalPropertiesFailure err)]
runAdditionalProperties _ _ _ val@(Bool v) =
  Just $ \x ->
    case x of
      Object y ->
        if not v && H.size y > 0
          then pure $ ValidationFailure AdditionalPropertiesBool (FailureInfo val x)
          else mempty
      _ -> mempty
runAdditionalProperties spec g s (Object o) =
  let sub = compile spec g (RawSchema (_rsURI s) o)
  in Just $ \x ->
    case x of
      Object y -> H.elems y >>= fmap (modifyFailureName AdditionalPropertiesObject) . validate sub
      _        -> mempty
runAdditionalProperties _ _ _ _ = Nothing
