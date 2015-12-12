{-# LANGUAGE OverloadedStrings #-}

module PrettyShowFailure where

import           Data.Aeson
import qualified Data.Aeson.Pointer     as AP
import           Data.Monoid

import qualified Data.JsonSchema.Draft4 as D4

badData :: Value
badData = toJSON [1, 2 :: Int]

failure :: D4.Failure
failure = D4.Failure
  { D4._failureValidatorsCalled = D4.Items D4.MultipleOf
  , D4._failureFinalValidator   = Number 2
  , D4._failureOffendingData    = AP.Pointer [AP.Token "0"]
  }

subsetOfData :: Value
subsetOfData =
  -- This is the only really interesting part of this example. To resolve the
  -- pointer to the part of the data that triggered the validation failure we
  -- have to have hjsonpointer in our build-depends and use 'AP.resolve'.
  case AP.resolve (D4._failureOffendingData failure) badData of
    Left _  -> error "Couldn't resolve pointer."
    Right v -> v

example :: IO ()
example = putStrLn . unlines $
  [ "Invalid data. Here's the sequence of validators that caught it:"
  , ""
  , "  " <> show (D4._failureValidatorsCalled failure)
  , ""
  , "Here's the contents of the final validator in that sequence:"
  , ""
  , "  " <> show (D4._failureFinalValidator failure)
  , ""
  , "Here's a JSON Pointer to the invalid part of the data:"
  , ""
  , "  " <> show (D4._failureOffendingData failure)
  , ""
  , "Here's the invalid part of the data:"
  , ""
  , "  " <> show subsetOfData
  ]
