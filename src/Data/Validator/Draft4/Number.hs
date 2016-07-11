
module Data.Validator.Draft4.Number where

import           Import
import           Prelude

import           Data.Fixed             (mod')
import           Data.Scientific        (Scientific)

import           Data.Validator.Failure (Fail(..))

--------------------------------------------------
-- * multipleOf
--------------------------------------------------

-- | The spec requires @"multipleOf"@ to be positive.
multipleOf :: Scientific -> Scientific -> Maybe (Fail ())
multipleOf n x
    | n <= 0          = Nothing
    | x `mod'` n /= 0 = Just (Failure () (toJSON n) mempty (Number x))
    | otherwise       = Nothing

--------------------------------------------------
-- * maximum
--------------------------------------------------

data MaximumInvalid
    = Maximum
    | ExclusiveMaximum
    deriving (Eq, Show)

maximumVal
    :: Bool
    -> Scientific
    -> Scientific
    -> Maybe (Fail MaximumInvalid)
maximumVal exclusive n x
    | x `greaterThan` n = Just (Failure err (toJSON n) mempty (Number x))
    | otherwise         = Nothing
  where
    (greaterThan, err) = if exclusive
                             then ((>=), ExclusiveMaximum)
                             else ((>), Maximum)

--------------------------------------------------
-- * minimum
--------------------------------------------------

data MinimumInvalid
    = Minimum
    | ExclusiveMinimum
    deriving (Eq, Show)

minimumVal
    :: Bool
    -> Scientific
    -> Scientific
    -> Maybe (Fail MinimumInvalid)
minimumVal exclusive n x
    | x `lessThan` n = Just (Failure err (toJSON n) mempty (Number x))
    | otherwise      = Nothing
  where
    (lessThan, err) = if exclusive
                          then ((<=), ExclusiveMinimum)
                          else ((<), Minimum)
