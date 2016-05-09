-- | 'D4.Invalid's contain a JSON Pointer to the subset of the data that
-- caused validation to fail, but they don't contain that data itself.
--
-- If you want to display the invalid subset of the data here's how you
-- resolve the JSON Pointer (which has the field name
-- 'D4._invalidOffendingData') against the original data.
--
-- NOTE: You have to have hjsonpointer in your build-depends.

module PrettyShowFailure where

import           Data.Aeson
import qualified Data.Aeson.Pointer     as AP
import           Data.Monoid

import qualified Data.JsonSchema.Draft4 as D4

badData :: Value
badData = toJSON [1, 2 :: Int]

failure :: D4.Invalid
failure = D4.Invalid
  { D4._invalidValidatorsCalled = D4.Items D4.MultipleOf
  , D4._invalidFinalValidator   = Number 2
  , D4._invalidOffendingData    = AP.Pointer [AP.Token "0"]
  }

example :: IO ()
example =
  case AP.resolve (D4._invalidOffendingData failure) badData of
    Left _  -> error "Couldn't resolve pointer."
    Right _ -> return () -- Success. We could feed the 'Right' value into
                         -- the otherwise unused 'msg' if we wanted to
                         -- display it.

msg :: Value -> String
msg subsetOfData = unlines
  [ "Invalid data. Here's the sequence of validators that caught it:"
  , ""
  , "  " <> show (D4._invalidValidatorsCalled failure)
  , ""
  , "Here's the contents of the final validator in that sequence:"
  , ""
  , "  " <> show (D4._invalidFinalValidator failure)
  , ""
  , "Here's a JSON Pointer to the invalid part of the data:"
  , ""
  , "  " <> show (D4._invalidOffendingData failure)
  , ""
  , "Here's the invalid part of the data:"
  , ""
  , "  " <> show subsetOfData
  ]
