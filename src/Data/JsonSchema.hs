{-# LANGUAGE TemplateHaskell #-}

module Data.JsonSchema
  ( module Data.JsonSchema.Core
  , module Data.JsonSchema
  ) where

import           Control.Applicative
import           Data.Aeson
import           Data.ByteString.Lazy       (fromStrict)
import           Data.FileEmbed
import           Data.Foldable
import qualified Data.HashMap.Strict        as H
import           Data.JsonSchema.Core
import           Data.JsonSchema.Helpers
import           Data.JsonSchema.Reference
import           Data.JsonSchema.Validators
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Prelude                    hiding (foldr)

--------------------------------------------------
-- * Draft 4 Specific
--------------------------------------------------

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

-- | Check if a 'RawSchema' is valid Draft 4 schema.
--
-- This is just a convenience function built by preloading 'validate'
-- with the spec schema that describes valid Draft 4 schemas.
--
-- NOTE: It's not actually required to run 'isValidSchema' on
-- prospective draft 4 schemas at all. However, it's a good way to
-- catch unintentional mistakes in schema documents.
isValidSchema :: RawSchema -> Either (Vector ValErr) Value
isValidSchema r =
  case decode . fromStrict $ $(embedFile "draft4.json") of
    Nothing -> Left $ V.singleton "Schema decode failed (this should never happen)"
    Just s  -> validate (compile draft4 H.empty $ RawSchema "" s) $ Object (_rsObject r)

-- | Check that a 'RawSchema' conforms to the JSON Schema Draft 4
-- master schema document. Compile it if it does.
--
-- This is just a convenience function built by combining
-- 'isValidSchema' and 'compile'.
compileDraft4 :: Graph -> RawSchema -> Either (Vector ValErr) Schema
compileDraft4 g r = isValidSchema r >> return (compile draft4 g r)

--------------------------------------------------
-- * Graph Builder
--------------------------------------------------
-- $ TODO: It would be nice to have a few other functions for building
-- out 'Graph's depending on the situation. For instance, there could be
-- a function that only fetched "$ref"s which use the "file://" scheme.

-- | Take a schema. Retrieve every document either it or its
-- subschemas include via the "$ref" keyword. Load a 'Graph' out
-- with them.
--
-- TODO: This function's URL processing is hacky and needs improvement.
fetchRefs :: Spec -> RawSchema -> Graph -> IO (Either Text Graph)
fetchRefs spec a graph =
  let startingGraph = H.insert (_rsURI a) (_rsObject a) graph
  in foldlM fetch (Right startingGraph) (includeSubschemas a)

  where
    includeSubschemas :: RawSchema -> Vector RawSchema
    includeSubschemas r =
      let newId = newResolutionScope (_rsURI r) (_rsObject r)
          xs = H.intersectionWith (\(_,f) x -> f newId x) (_unSpec spec) (_rsObject r)
          ys = V.concat . H.elems $ xs -- TODO: optimize
      in V.cons r . V.concat . V.toList $ includeSubschemas <$> ys

    fetch :: Either Text Graph -> RawSchema -> IO (Either Text Graph)
    fetch (Right g) r =
      case H.lookup "$ref" (_rsObject r) >>= toTxt >>= refAndPointer of
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
