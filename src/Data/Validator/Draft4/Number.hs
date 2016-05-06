
module Data.Validator.Draft4.Number where

import           Data.Fixed             (mod')
import           Data.Scientific

import           Data.Validator.Failure
import           Import

-- | The spec requires "multipleOf" to be positive.
multipleOf :: Scientific -> Scientific -> Maybe (Failure ())
multipleOf n x
  | n <= 0          = Nothing
  | x `mod'` n /= 0 = Just (Invalid () (toJSON n) mempty)
  | otherwise       = Nothing

data MaximumInvalid
  = Maximum
  | ExclusiveMaximum
  deriving (Eq, Show)

maximumVal
  :: Bool
  -> Scientific
  -> Scientific
  -> Maybe (Failure MaximumInvalid)
maximumVal exclusiveMaximum n x
  | x `greaterThan` n = Just (Invalid err (toJSON n) mempty)
  | otherwise         = Nothing
  where
    (greaterThan, err) = if exclusiveMaximum
                           then ((>=), ExclusiveMaximum)
                           else ((>), Maximum)

data MinimumInvalid
  = Minimum
  | ExclusiveMinimum
  deriving (Eq, Show)

minimumVal
  :: Bool
  -> Scientific
  -> Scientific
  -> Maybe (Failure MinimumInvalid)
minimumVal exclusiveMinimum n x
  | x `lessThan` n = Just (Invalid err (toJSON n) mempty)
  | otherwise      = Nothing
  where
    (lessThan, err) = if exclusiveMinimum
                        then ((<=), ExclusiveMinimum)
                        else ((<), Minimum)
