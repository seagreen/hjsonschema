module Data.JsonSchema.Utils where

import           Data.Aeson
import           Data.HashMap.Strict  (HashMap)
import           Data.JsonSchema.Core
import           Data.List
import           Data.Monoid
import           Data.Scientific
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Vector          (Vector)
import qualified Data.Vector          as V

isJsonType :: Value -> Vector Text -> Vector ValErr
isJsonType x xs =
  case x of
    (Null)     -> f "null"    xs ("null" :: Text)
    (Array y)  -> f "array"   xs y
    (Bool y)   -> f "boolean" xs y
    (Object y) -> f "object"  xs y
    (String y) -> f "string"  xs y
    (Number y) ->
      case toBoundedInteger y :: Maybe Int of
        Nothing -> f "number" xs y
        Just _  -> if V.elem "number" xs || V.elem "integer" xs
                     then mempty
                     else mkErr y xs
  where
    f :: (Show a) => Text -> Vector Text -> a -> Vector ValErr
    f t ts d = if V.elem t ts then mempty else mkErr d ts

    mkErr :: (Show a) => a -> Vector Text -> Vector ValErr
    mkErr y ts = V.singleton $ tshow y <> " is not one of the types " <> tshow ts

runMaybeVal :: Maybe Validator -> Value -> Vector ValErr
runMaybeVal Nothing _ = mempty
runMaybeVal (Just val) d = val d

runMaybeVal'
  :: Maybe (Value -> (Vector ValErr, Value))
  -> Value
  -> (Vector ValErr, Value)
runMaybeVal' Nothing d = (mempty, d)
runMaybeVal' (Just val) d = val d

-- TODO: optimize
-- see here: http://comments.gmane.org/gmane.comp.lang.haskell.cafe/106242
allUnique :: (Eq a) => Vector a -> Bool
allUnique bs = length (nub (V.toList bs)) == V.length bs

-- TODO: optimize
count :: (Eq a) => a -> Vector a -> Int
count b bs = V.length $ V.filter (== b) bs

toObj :: Value -> Maybe (HashMap Text Value)
toObj (Object a) = Just a
toObj _ = Nothing

fromJSONInt :: Value -> Maybe Int
fromJSONInt (Number n) = toBoundedInteger n
fromJSONInt _ = Nothing

toTxt :: Value -> Maybe Text
toTxt (String t) = Just t
toTxt _ = Nothing

greaterThanZero :: (Num a, Ord a) => a -> Maybe ()
greaterThanZero n = if n <= 0 then Nothing else Just ()

tshow :: Show a => a -> Text
tshow = T.pack . show
