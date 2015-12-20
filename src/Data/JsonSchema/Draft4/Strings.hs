
module Data.JsonSchema.Draft4.Strings where

import qualified Data.Text               as T
import           Text.RegexPR

import           Data.JsonSchema.Core
import           Data.JsonSchema.Helpers
import           Import

maxLength :: ValidatorConstructor err [FailureInfo]
maxLength _ _ _ val path = do
  n <- fromJSONInt val
  greaterThanZero n
  Just $ \x ->
    case x of
      String y ->
        if T.length y > n
          then pure (FailureInfo val x path)
          else mempty
      _ -> mempty

minLength :: ValidatorConstructor err [FailureInfo]
minLength _ _ _ val path = do
  n <- fromJSONInt val
  greaterThanZero n
  Just $ \x ->
    case x of
      String y ->
        if T.length y < n
          then pure (FailureInfo val x path)
          else mempty
      _ -> mempty

pattern :: ValidatorConstructor err [FailureInfo]
pattern _ _ _ val@(String t) path =
  Just $ \x ->
    case x of
      String y ->
        case matchRegexPR (T.unpack t) (T.unpack y) of
          Nothing -> pure (FailureInfo val x path)
          Just _  -> mempty
      _ -> mempty
pattern _ _ _ _ _ = Nothing
