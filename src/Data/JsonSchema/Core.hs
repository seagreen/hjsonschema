module Data.JsonSchema.Core where

import           Data.Aeson
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as H
import           Data.JsonSchema.JsonReference
import           Data.Maybe
import           Data.Text                     (Text)
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as V

newtype Spec = Spec { _unSpec :: HashMap Text ValidatorGen }

-- | Set of potentially mutually recursive schemas.
type Graph = HashMap Text (HashMap Text Value)

type ValErr = Text

type Validator = Value -> Vector ValErr

type ValidatorGen = Spec -> Graph -> RawSchema -> Value -> Maybe Validator

type Schema = Vector Validator

data RawSchema = RawSchema
  { _rsURI    :: Text
  , _rsObject :: HashMap Text Value
  }

compile :: Spec -> Graph -> RawSchema -> Schema
compile spec g (RawSchema t o) =
  V.fromList . catMaybes . H.elems $ H.intersectionWith f (_unSpec spec) o
  where
    f :: ValidatorGen -> Value -> Maybe Validator
    f vGen = vGen spec g $ RawSchema (updateId t o) o

validate :: Schema -> Value -> Vector ValErr
validate s x = s >>= ($ x)
