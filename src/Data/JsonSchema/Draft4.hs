{-# LANGUAGE TemplateHaskell #-}

module Data.JsonSchema.Draft4 where

import qualified Data.ByteString.Lazy           as LBS
import           Data.FileEmbed
import qualified Data.HashMap.Strict            as H

import           Data.JsonSchema.Core
import           Data.JsonSchema.Draft4.Any
import qualified Data.JsonSchema.Draft4.Arrays as AR
import qualified Data.JsonSchema.Draft4.Numbers as NU
import qualified Data.JsonSchema.Draft4.Objects as OB
import           Data.JsonSchema.Draft4.Strings
import           Data.JsonSchema.Helpers
import           Import

data Draft4Failure
  = MultipleOf

  | Maximum
  | ExclusiveMaximum

  | Minimum
  | ExclusiveMinimum

  | MaxLength
  | MinLength
  | Pattern

  | Items Draft4Failure
  | AdditionalItemsBool
  | AdditionalItemsObject Draft4Failure

  | MaxItems
  | MinItems
  | UniqueItems
  | MaxProperties
  | MinProperties
  | Required

  | Properties Draft4Failure
  | PatternProperties Draft4Failure
  | AdditionalPropertiesBool
  | AdditionalPropertiesObject Draft4Failure

  | SchemaDependency Draft4Failure
  | PropertyDependency

  | Enum
  | TypeValidator
  | AllOf Draft4Failure
  | AnyOf
  | OneOf
  | NotValidator
  | Ref Draft4Failure
  deriving (Eq, Show, Read)

draft4 :: Spec Draft4Failure
draft4 = Spec $ H.fromList
  [ ("$ref"                , ValSpec noEm             (modifyName Ref           ref))
  , ("multipleOf"          , ValSpec noEm             (giveName   MultipleOf    NU.multipleOf))
  , ("maximum"             , ValSpec noEm             (modifyName fMax          NU.maximumVal))
  , ("minimum"             , ValSpec noEm             (modifyName fMin          NU.minimumVal))
  , ("maxLength"           , ValSpec noEm             (giveName   MaxLength     maxLength))
  , ("minLength"           , ValSpec noEm             (giveName   MinLength     minLength))
  , ("pattern"             , ValSpec noEm             (giveName   Pattern       pattern))
  , ("additionalItems"     , ValSpec objEmbed         neverBuild) -- Handled by items.
  , ("items"               , ValSpec objOrArrayEmbed  (modifyName fItems        AR.items))
  , ("maxItems"            , ValSpec noEm             (giveName   MaxItems      AR.maxItems))
  , ("minItems"            , ValSpec noEm             (giveName   MinItems      AR.minItems))
  , ("uniqueItems"         , ValSpec noEm             (giveName   UniqueItems   AR.uniqueItems))
  , ("maxProperties"       , ValSpec noEm             (giveName   MaxProperties OB.maxProperties))
  , ("minProperties"       , ValSpec noEm             (giveName   MinProperties OB.minProperties))
  , ("required"            , ValSpec noEm             (giveName   Required      OB.required))
  , ("properties"          , ValSpec objMembersEmbed  (modifyName fProp         OB.properties))
  , ("patternProperties"   , ValSpec objMembersEmbed  (modifyName fPatProp      OB.patternProperties))
  , ("additionalProperties", ValSpec objEmbed         (modifyName fAddProp      OB.additionalProperties))
  , ("dependencies"        , ValSpec objMembersEmbed  (modifyName fDeps         OB.dependencies))
  , ("enum"                , ValSpec noEm             (giveName   Enum          enum))
  , ("type"                , ValSpec noEm             (giveName   TypeValidator typeValidator))
  , ("allOf"               , ValSpec arrayEmbed       (modifyName AllOf         allOf))
  , ("anyOf"               , ValSpec arrayEmbed       (giveName   AnyOf         anyOf))
  , ("oneOf"               , ValSpec arrayEmbed       (giveName   OneOf         oneOf))
  , ("not"                 , ValSpec objEmbed         (giveName   NotValidator  notValidator))
  , ("definitions"         , ValSpec objMembersEmbed  neverBuild) -- Just contains referenceable schemas.
  ]
  where
    fMax NU.Maximum = Maximum
    fMax NU.ExclusiveMaximum = ExclusiveMaximum

    fMin NU.Minimum = Minimum
    fMin NU.ExclusiveMinimum = ExclusiveMinimum

    fItems (AR.Items err) = Items err
    fItems AR.AdditionalItemsBool = AdditionalItemsBool
    fItems (AR.AdditionalItemsObject err) = AdditionalItemsObject err

    fProp (OB.Properties err) = Properties err
    fProp (OB.PropPattern err) = PatternProperties err
    fProp (OB.PropAdditional OB.AdditionalPropertiesBool) = AdditionalPropertiesBool
    fProp (OB.PropAdditional (OB.AdditionalPropertiesObject err)) = AdditionalPropertiesObject err

    fPatProp (OB.PatternProperties err) = PatternProperties err
    fPatProp (OB.PatternAdditional OB.AdditionalPropertiesBool) = AdditionalPropertiesBool
    fPatProp (OB.PatternAdditional (OB.AdditionalPropertiesObject err)) = AdditionalPropertiesObject err

    fAddProp OB.AdditionalPropertiesBool = AdditionalPropertiesBool
    fAddProp (OB.AdditionalPropertiesObject err) = AdditionalItemsObject err

    fDeps (OB.SchemaDependency err) = SchemaDependency err
    fDeps OB.PropertyDependency = PropertyDependency

-- | Check if a 'RawSchema' is valid Draft 4 schema.
--
-- This is just a convenience function built by preloading 'validate'
-- with the spec schema that describes valid Draft 4 schemas.
--
-- NOTE: It's not actually required to run 'isValidSchema' on
-- prospective draft 4 schemas at all. However, it's a good way to
-- catch unintentional mistakes in schema documents.
isValidSchema :: RawSchema -> [ValidationFailure Draft4Failure]
isValidSchema r =
  case decode . LBS.fromStrict $ $(embedFile "draft4.json") of
    Nothing -> error "Schema decode failed (this should never happen)"
    Just s  ->
      let a = RawSchema Nothing s
      in validate (compile draft4 (SchemaGraph a mempty) a) $ Object (_rsData r)

-- | Check that a 'RawSchema' conforms to the JSON Schema Draft 4
-- master schema document. Compile it if it does.
--
-- This is just a convenience function built by combining
-- 'isValidSchema' and 'compile'.
--
-- NOTE: It's not actually required to run 'isValidSchema' on
-- prospective draft 4 schemas at all.
compileDraft4 :: SchemaGraph -> RawSchema -> Either [ValidationFailure Draft4Failure] (Schema Draft4Failure)
compileDraft4 g r =
  case isValidSchema r of
    []   -> Right (compile draft4 g r)
    errs -> Left errs
