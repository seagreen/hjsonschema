
module Data.JsonSchema.Draft4.Numbers where

import           Data.Fixed              (mod')
import qualified Data.HashMap.Strict     as H
import           Data.Scientific

import           Data.JsonSchema.Core
import           Data.JsonSchema.Helpers
import           Import

data MaximumFailure = Maximum | ExclusiveMaximum
data MinimumFailure = Minimum | ExclusiveMinimum

multipleOf :: ValidatorConstructor err [FailureInfo]
multipleOf _ _ _ val@(Number n) _ = do
  greaterThanZero n
  Just $ \x ->
    case x of
      Number y ->
        if y `mod'` n /= 0
          then pure (FailureInfo val x mempty)
          else mempty
      _ -> mempty
multipleOf _ _ _ _ _ = Nothing

maximumVal :: ValidatorConstructor err [ValidationFailure MaximumFailure]
maximumVal _ _ s val@(Number n) _ =
  Just $ \x ->
    case x of
      Number y ->
        let (greater, err) = checkExclusive
        in if y `greater` n
          then pure $ ValidationFailure err (FailureInfo val x mempty)
          else mempty
      _ -> mempty
  where
    checkExclusive :: (Scientific -> Scientific -> Bool, MaximumFailure)
    checkExclusive =
      case H.lookup "exclusiveMaximum" (_rsData s) of
        Just (Bool a) -> if a then ((>=), ExclusiveMaximum) else ((>), Maximum)
        _             -> ((>), Maximum)
maximumVal _ _ _ _ _ = Nothing

minimumVal :: ValidatorConstructor err [ValidationFailure MinimumFailure]
minimumVal _ _ s val@(Number n) _ =
  Just $ \x ->
    case x of
      Number y ->
        let (lesser, err) = checkExclusive
        in if y `lesser` n
          then pure $ ValidationFailure err (FailureInfo val x mempty)
          else mempty
      _ -> mempty
  where
    checkExclusive :: (Scientific -> Scientific -> Bool, MinimumFailure)
    checkExclusive =
      case H.lookup "exclusiveMinimum" (_rsData s) of
        Just (Bool a) -> if a then ((<=), ExclusiveMinimum) else ((<), Minimum)
        _             -> ((<), Minimum)
minimumVal _ _ _ _ _ = Nothing
