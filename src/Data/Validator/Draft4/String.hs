
module Data.Validator.Draft4.String where

import           Data.Aeson
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import qualified Text.Regex.PCRE.Heavy  as RE

import           Data.Validator.Failure
import           Import

-- | The spec requires "maxLength" to be non-negative.
maxLength :: Int -> Text -> Maybe (Failure ())
maxLength n x
  | n <= 0         = Nothing
  | T.length x > n = Just (Failure () (toJSON n) mempty)
  | otherwise      = Nothing

-- | The spec requires "minLength" to be non-negative.
minLength :: Int -> Text -> Maybe (Failure ())
minLength n x
  | n <= 0         = Nothing
  | T.length x < n = Just (Failure () (toJSON n) mempty)
  | otherwise      = Nothing

patternVal :: Text -> Text -> Maybe (Failure ())
patternVal t x =
  case RE.compileM (encodeUtf8 t) mempty of
    Left _   -> Just (Failure () (toJSON t) mempty)
    Right re -> if x RE.=~ re
                  then Nothing
                  else Just (Failure () (toJSON t) mempty)
