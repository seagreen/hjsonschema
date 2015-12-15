
module Data.JsonSchema.Draft4.Strings where

import qualified Data.Text               as T
import           Text.RegexPR

import           Data.JsonSchema.Core
import           Data.JsonSchema.Helpers
import           Import

maxLength :: ValidatorConstructor err [FailureInfo]
maxLength _ _ _ val _ = do
  n <- fromJSONInt val
  greaterThanZero n
  Just $ \x ->
    case x of
      String y ->
        if T.length y > n
          then pure (FailureInfo val x mempty)
          else mempty
      _ -> mempty

minLength :: ValidatorConstructor err [FailureInfo]
minLength _ _ _ val _ = do
  n <- fromJSONInt val
  greaterThanZero n
  Just $ \x ->
    case x of
      String y ->
        if T.length y < n
          then pure (FailureInfo val x mempty)
          else mempty
      _ -> mempty

pattern :: ValidatorConstructor err [FailureInfo]
pattern _ _ _ val@(String t) _ =
  Just $ \x ->
    case x of
      String y ->
        case matchRegexPR (T.unpack t) (T.unpack y) of
          Nothing -> pure (FailureInfo val x mempty)
          Just _  -> mempty
      _ -> mempty
pattern _ _ _ _ _ = Nothing
