
module Data.Validator.Draft4.String where

import           Import
import           Prelude

import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import qualified Text.Regex.PCRE.Heavy  as RE

import           Data.Validator.Failure (Fail(..))

--------------------------------------------------
-- * maxLength
--------------------------------------------------

-- | The spec requires @"maxLength"@ to be non-negative.
maxLength :: Int -> Text -> Maybe (Fail ())
maxLength n x
    | n <= 0         = Nothing
    | T.length x > n = Just (Failure () (toJSON n) mempty (String x))
    | otherwise      = Nothing

--------------------------------------------------
-- * minLength
--------------------------------------------------

-- | The spec requires @"minLength"@ to be non-negative.
minLength :: Int -> Text -> Maybe (Fail ())
minLength n x
    | n <= 0         = Nothing
    | T.length x < n = Just (Failure () (toJSON n) mempty (String x))
    | otherwise      = Nothing

--------------------------------------------------
-- * pattern
--------------------------------------------------

patternVal :: Text -> Text -> Maybe (Fail ())
patternVal t x =
    case RE.compileM (encodeUtf8 t) mempty of
        Left _   -> Just (Failure () (toJSON t) mempty (String x))
        Right re -> if x RE.=~ re
                        then Nothing
                        else Just (Failure () (toJSON t) mempty (String x))
