module Data.JsonSchema
  ( module Data.JsonSchema
  , module Data.JsonSchema.Core
  ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Foldable
import qualified Data.HashMap.Strict           as H
import           Data.JsonSchema.Core
import           Data.JsonSchema.JsonReference
import           Data.JsonSchema.Utils
import           Data.JsonSchema.Validators
import           Data.Maybe
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as V
import           Prelude                       hiding (foldr)

draft4 :: Spec
draft4 = Spec $ H.fromList
  [ ("$ref", ref)
  , ("multipleOf", multipleOf)
  , ("maximum", maximumVal)
  , ("minimum", minimumVal)
  , ("maxLength", maxLength)
  , ("minLength", minLength)
  , ("pattern", pattern)
  , ("items", items)
  , ("maxItems", maxItems)
  , ("minItems", minItems)
  , ("uniqueItems", uniqueItems)
  , ("maxProperties", maxProperties)
  , ("minProperties", minProperties)
  , ("required", required)
  , ("properties", properties)
  , ("patternProperties", patternProperties)
  , ("additionalProperties", additionalProperties)
  , ("dependencies", dependencies)
  , ("enum", enum)
  , ("type", typeVal)
  , ("allOf", allOf)
  , ("anyOf", anyOf)
  , ("oneOf", oneOf)
  , ("not", notValidator)
  ]

fetchRefs :: RawSchema -> Graph -> IO Graph
fetchRefs a graph =
  let newGraph = H.insert (_rsURI a) (_rsObject a) graph
      rSchema = allEmbedded (_rsURI a, Object $ _rsObject a) V.empty
  in foldlM fetch newGraph rSchema

  where
    -- TODO: optimize
    allEmbedded :: (Text, Value) -> Vector RawSchema -> Vector RawSchema
    allEmbedded (t, Object o) rs =
      let t' = updateId t o
          r = RawSchema t' o
          ns = (\x -> (t',x)) <$> V.fromList (H.elems o)
      in foldr allEmbedded (V.snoc rs r) ns
    allEmbedded (t, Array vs) rs =
      let ns = (\x -> (t,x)) <$> vs
      in foldr allEmbedded rs ns
    allEmbedded _ rs = rs

    fetch :: Graph -> RawSchema -> IO Graph
    fetch g r =
      case H.lookup "$ref" (_rsObject r) >>= toTxt >>= refAndP of
        Nothing     -> return g
        Just (s, _) ->
          let t = (_rsURI r `combineIdAndRef` s)
          in if T.length t <= 0 || H.member t g || not ("://" `T.isInfixOf` t)
            then return g
            else do
              eResp <- fetchRef t
              case eResp of
                Right obj -> fetchRefs (RawSchema t obj) g
                _         -> return g
