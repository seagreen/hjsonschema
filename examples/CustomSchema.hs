{-# LANGUAGE OverloadedStrings #-}

-- | A custom schema made up of one validator from 'Data.Validator.Draft4'
-- and one original validator.
--
-- This is a simple example because it doesn't allow references (so it
-- doesn't need to define an 'embed' function @Schema -> [Schema]@ for use
-- with 'fetchReferencedSchemas'.

module CustomSchema where

import           Control.Applicative
import           Data.Aeson
import           Data.Maybe             (maybeToList)
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T

import qualified Data.Validator.Draft4  as VA
import qualified Data.Validator.Failure as FR

-- | Our custom validator.
oddLength :: Bool -> Text -> Maybe (FR.Failure () )
oddLength b t
  | b == odd (T.length t) = Nothing
  | otherwise             = Just (FR.Failure () (Bool b) mempty)

data CustomError
  = MaxLength
  | OddLength

-- If we were really using the schema we would also need ToJSON and FromJSON
-- instances.
data Schema = Schema
  { _schemaMaxLength :: Maybe Int
  , _schemaOddLength :: Maybe Bool
  }

-- | Since every 'Schema' is valid we don't need to bother defining something
-- like 'Data.JsonSchema.Draft4.checkValidity' for this schema.
validate :: Schema -> Value -> [FR.Failure CustomError]
validate s (String x) = concat
  [ f _schemaMaxLength (FR.setFailure MaxLength) (fmap maybeToList . VA.maxLength)
  , f _schemaOddLength (FR.setFailure OddLength) (fmap maybeToList . oddLength)
  ]
  where
    -- This pattern is overkill here, but is helpful if you have lots of
    -- validators (e.g. the Draft 4 schema has 27).
    f :: (Schema -> Maybe val)
      -> (err -> FR.Failure CustomError)
      -> (val -> Text -> [err])
      -> [FR.Failure CustomError]
    f field modifyError runVal =
      maybe mempty (\val -> modifyError <$> runVal val x) (field s)
validate _ _ = mempty -- Our schema passes everything that isn't a string.

example :: IO ()
example =
  case validate schema badData of
    [] -> error "We validated bad data."
    [FR.Failure OddLength _ _] -> putStrLn "Success."
    _ -> error "We got a different failure than expected."
  where
    schema :: Schema
    schema = Schema (Just 100) (Just True)

    badData :: Value
    badData = String "even"
