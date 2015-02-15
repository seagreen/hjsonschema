module Data.JsonSchema
  ( module Data.JsonSchema
  , module Data.JsonSchema.Core
  ) where

import           Control.Applicative
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
  [ ("$ref"                , (ref                 , noEm))
  , ("multipleOf"          , (multipleOf          , noEm))
  , ("maximum"             , (maximumVal          , noEm))
  , ("minimum"             , (minimumVal          , noEm))
  , ("maxLength"           , (maxLength           , noEm))
  , ("minLength"           , (minLength           , noEm))
  , ("pattern"             , (pattern             , noEm))
  , ("additionalItems"     , (noVal               , objEmbed))
  , ("items"               , (items               , objOrArrayEmbed))
  , ("maxItems"            , (maxItems            , noEm))
  , ("minItems"            , (minItems            , noEm))
  , ("uniqueItems"         , (uniqueItems         , noEm))
  , ("maxProperties"       , (maxProperties       , noEm))
  , ("minProperties"       , (minProperties       , noEm))
  , ("required"            , (required            , noEm))
  , ("properties"          , (properties          , objMembersEmbed))
  , ("patternProperties"   , (patternProperties   , objMembersEmbed))
  , ("additionalProperties", (additionalProperties, objEmbed))
  , ("dependencies"        , (dependencies        , objMembersEmbed))
  , ("enum"                , (enum                , noEm))
  , ("type"                , (typeVal             , noEm))
  , ("allOf"               , (allOf               , arrayEmbed))
  , ("anyOf"               , (anyOf               , arrayEmbed))
  , ("oneOf"               , (oneOf               , arrayEmbed))
  , ("not"                 , (notValidator        , objEmbed))
  , ("definitions"         , (noVal               , objMembersEmbed))
  ]

fetchRefs :: Spec -> RawSchema -> Graph -> IO (Either Text Graph)
fetchRefs spec a graph =
  let startingGraph = H.insert (_rsURI a) (_rsObject a) graph
  in foldlM fetch (Right startingGraph) (includeSubschemas a)

  where
    -- TODO: optimize
    includeSubschemas :: RawSchema -> Vector RawSchema
    includeSubschemas r =
      let newId = updateId (_rsURI r) (_rsObject r)
          xs = H.intersectionWith (\(_,f) x -> f newId x) (_unSpec spec) (_rsObject r)
          ys = V.concat . H.elems $ xs
      in V.cons r . V.concat . V.toList $ includeSubschemas <$> ys

    fetch :: Either Text Graph -> RawSchema -> IO (Either Text Graph)
    fetch (Right g) r =
      case H.lookup "$ref" (_rsObject r) >>= toTxt >>= refAndP of
        Nothing     -> return (Right g)
        Just (s, _) ->
          let t = (_rsURI r `combineIdAndRef` s)
          in if T.length t <= 0 || H.member t g || not ("://" `T.isInfixOf` t)
            then return (Right g)
            else do
              eResp <- fetchRef t
              case eResp of
                Left e    -> return (Left e)
                Right obj -> fetchRefs spec (RawSchema t obj) g
    fetch leftGraph _ = return leftGraph
