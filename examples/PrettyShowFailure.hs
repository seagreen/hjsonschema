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

example :: IO ()
example =
  -- 'D4.Failure's contain the sequence of embedded validators that caused
  -- the invalidation, the contents of the final validator in that sequence,
  -- and a JSON Pointer to the subset of data that caused validation to fail.
  --
  -- They don't actually contain that subset of data, since it can be derived
  -- from the original data and the JSON Pointer.
  --
  -- If you want to display the invalid subset of the data here's how you
  -- resolve the JSON Pointer ('D4._failureOffendingData') against the
  -- original data.
  --
  -- NOTE: You have to have hjsonpointer in your build-depends.
  case AP.resolve (D4._failureOffendingData failure) badData of
    Left _  -> error "Couldn't resolve pointer."
    Right _ -> putStrLn "Success." -- We could feed the 'Right' value into
                                   -- 'msg' if we wanted to display it.

msg :: Value -> String
msg subsetOfData = unlines
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
