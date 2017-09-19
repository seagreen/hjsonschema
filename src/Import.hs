
module Import (module Export, fromJSONEither) where

import           Protolude           as Export

import           Data.Aeson          as Export
import           Data.HashMap.Strict as Export (HashMap)
import           Data.List.NonEmpty  as Export (NonEmpty)
import           Data.Vector         as Export (Vector)
import           Test.QuickCheck     as Export hiding ((.&.), Failure,
                                                       Result, Success)

import qualified Data.Text           as T

fromJSONEither :: FromJSON a => Value -> Either Text a
fromJSONEither a =
    case fromJSON a of
        Error e   -> Left (T.pack e)
        Success b -> Right b
