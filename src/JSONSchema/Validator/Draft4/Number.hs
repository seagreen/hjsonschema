
module JSONSchema.Validator.Draft4.Number where

import           Import

import           Data.Fixed      (mod')
import           Data.Scientific (Scientific)

--------------------------------------------------
-- * multipleOf
--------------------------------------------------

newtype MultipleOf
    = MultipleOf { _unMultipleOf :: Scientific }
    deriving (Eq, Show)

instance FromJSON MultipleOf where
    parseJSON = withObject "MultipleOf" $ \o ->
        MultipleOf <$> o .: "multipleOf"

data MultipleOfInvalid
    = MultipleOfInvalid MultipleOf Scientific
    deriving (Eq, Show)

-- | The spec requires @"multipleOf"@ to be positive.
multipleOfVal :: MultipleOf -> Scientific -> Maybe MultipleOfInvalid
multipleOfVal a@(MultipleOf n) x
    | n <= 0          = Nothing
    | x `mod'` n /= 0 = Just (MultipleOfInvalid a x)
    | otherwise       = Nothing

--------------------------------------------------
-- * maximum
--------------------------------------------------

data Maximum = Maximum
    { _maximumExclusive :: Bool
    , _maximumValue     :: Scientific
    } deriving (Eq, Show)

instance FromJSON Maximum where
    parseJSON = withObject "Maximum" $ \o -> Maximum
        <$> o .:! "exclusiveMaximum" .!= False
        <*> o .: "maximum"

data MaximumInvalid
    = MaximumInvalid Maximum Scientific
    deriving (Eq, Show)

maximumVal
    :: Maximum
    -> Scientific
    -> Maybe MaximumInvalid
maximumVal a@(Maximum exclusive n) x
    | exclusive && x >= n = Just (MaximumInvalid a x)
    | x > n               = Just (MaximumInvalid a x)
    | otherwise           = Nothing

--------------------------------------------------
-- * minimum
--------------------------------------------------

data Minimum = Minimum
    { _minimumExclusive :: Bool
    , _minimumValue     :: Scientific
    } deriving (Eq, Show)

instance FromJSON Minimum where
    parseJSON = withObject "Minimum" $ \o -> Minimum
        <$> o .:! "exclusiveMinimum" .!= False
        <*> o .: "minimum"

data MinimumInvalid
    = MinimumInvalid Minimum Scientific
    deriving (Eq, Show)

minimumVal
    :: Minimum
    -> Scientific
    -> Maybe MinimumInvalid
minimumVal a@(Minimum exclusive n) x
    | exclusive && x <= n = Just (MinimumInvalid a x)
    | x < n               = Just (MinimumInvalid a x)
    | otherwise           = Nothing
