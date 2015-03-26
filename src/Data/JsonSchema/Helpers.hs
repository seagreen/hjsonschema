module Data.JsonSchema.Helpers where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Hashable
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as H
import           Data.JsonSchema.Core
import           Data.List
import           Data.Monoid
import           Data.Scientific
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Traversable
import           Data.Vector          (Vector)
import qualified Data.Vector          as V
import           Text.RegexPR

--------------------------------------------------
-- * Embedded Schema Layouts
--------------------------------------------------

noEm :: EmbeddedSchemas
noEm _ _ = V.empty

objEmbed :: EmbeddedSchemas
objEmbed t (Object o) = V.singleton $ RawSchema t o
objEmbed _ _ = V.empty

-- TODO: optimize
arrayEmbed :: EmbeddedSchemas
arrayEmbed t (Array vs) = V.concat . V.toList $ objEmbed t <$> vs
arrayEmbed _ _ = V.empty

objOrArrayEmbed :: EmbeddedSchemas
objOrArrayEmbed t v@(Object _) = objEmbed t v
objOrArrayEmbed t v@(Array _) = arrayEmbed t v
objOrArrayEmbed _ _ = V.empty

objMembersEmbed :: EmbeddedSchemas
objMembersEmbed t (Object o) = V.concat $ objEmbed t <$> H.elems o
objMembersEmbed _ _ = V.empty

--------------------------------------------------
-- * Validator Helpers
--------------------------------------------------

propertiesMatches
  :: Spec
  -> Graph
  -> RawSchema
  -> Value
  -> Maybe (Value -> (Vector ValErr, Value))
propertiesMatches spec g s (Object val) = do
  os <- traverse toObj val
  let oss = compile spec g . RawSchema (_rsURI s) <$> os
  Just (\x ->
    case x of
      Object y -> ( join . vectorOfElems $ H.intersectionWith validate oss y
                  , Object $ H.difference y oss)
      z        -> (mempty, z))
propertiesMatches _ _ _ _ = Nothing

patternPropertiesMatches
  :: Spec
  -> Graph
  -> RawSchema
  -> Value
  -> Maybe (Value -> (Vector ValErr, Value))
patternPropertiesMatches spec g s (Object val) = do
  os <- traverse toObj val
  let vs = compile spec g . RawSchema (_rsURI s) <$> os
  Just (\x ->
    case x of
      Object y -> let ms = matches (hmToVector vs) <$> hmToVector y
                  in (ms >>= runVals, leftovers ms)
      _        -> (mempty, x))
  where
    matches
      :: Vector (Text, Schema)
      -> (Text, Value)
      -> (Text, Value, Vector Schema)
    matches ss (k, v) = (k, v, ss >>= match k)

    match :: Text -> (Text, Schema) -> Vector Schema
    match k (r, sc) =
      case matchRegexPR (T.unpack r) (T.unpack k) of
        Nothing -> mempty
        Just _  -> V.singleton sc

    runVals :: (Text, Value, Vector Schema) -> Vector ValErr
    runVals (_,v,ss) = join $ validate <$> ss <*> pure v

    leftovers :: Vector (Text, Value, Vector Schema) -> Value
    leftovers possiblyMatched =
      let unmatched = V.filter (\(_,_,ss) -> V.length ss == 0) possiblyMatched
      in Object . vectorToHm $ (\(v,k,_) -> (v,k)) <$> unmatched

patternPropertiesMatches _ _ _ _ = Nothing

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

--------------------------------------------------
-- * Utils
--------------------------------------------------

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a

vectorOfElems :: HashMap k a -> Vector a
vectorOfElems = V.fromList . H.elems

hmToVector :: HashMap k a -> Vector (k, a)
hmToVector = V.fromList . H.toList

vectorToHm :: (Eq k, Hashable k) => Vector (k, a) -> HashMap k a
vectorToHm = H.fromList . V.toList

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
