
module Data.JsonSchema.Draft4.Schema where

import           Import
import           Prelude

import qualified Data.HashMap.Strict          as HM
import           Data.List.NonEmpty           (NonEmpty)
import           Data.Maybe                   (fromJust, isJust)
import           Data.Scientific

import qualified Data.Validator.Draft4.Any    as AN
import qualified Data.Validator.Draft4.Array  as AR
import qualified Data.Validator.Draft4.Object as OB
import           Data.Validator.Utils

data Schema = Schema
    { _schemaVersion              :: Maybe Text
    , _schemaId                   :: Maybe Text
    , _schemaRef                  :: Maybe Text
    , _schemaDefinitions          :: Maybe (HashMap Text Schema)
    -- ^ A standardized location for embedding schemas
    -- to be referenced from elsewhere in the document.
    , _schemaOther                :: HashMap Text Value
    -- ^ Since the JSON document this schema was built from could
    -- contain schemas anywhere (not just in "definitions" or any
    -- of the other official keys) we save any leftover key/value
    -- pairs not covered by them here.

    , _schemaMultipleOf           :: Maybe Scientific
    , _schemaMaximum              :: Maybe Scientific
    , _schemaExclusiveMaximum     :: Maybe Bool
    , _schemaMinimum              :: Maybe Scientific
    , _schemaExclusiveMinimum     :: Maybe Bool

    , _schemaMaxLength            :: Maybe Int
    , _schemaMinLength            :: Maybe Int
    , _schemaPattern              :: Maybe Text

    , _schemaMaxItems             :: Maybe Int
    , _schemaMinItems             :: Maybe Int
    , _schemaUniqueItems          :: Maybe Bool
    , _schemaItems                :: Maybe (AR.Items Schema)
    -- Note that '_schemaAdditionalItems' is left out of 'runValidate'
    -- because its functionality is handled by '_schemaItems'. It always
    -- validates data unless 'Items' is present.
    , _schemaAdditionalItems      :: Maybe (AR.AdditionalItems Schema)

    , _schemaMaxProperties        :: Maybe Int
    , _schemaMinProperties        :: Maybe Int
    , _schemaRequired             :: Maybe OB.Required
    , _schemaDependencies         :: Maybe (HashMap Text (OB.Dependency Schema))
    , _schemaProperties           :: Maybe (HashMap Text Schema)
    , _schemaPatternProperties    :: Maybe (HashMap Text Schema)
    , _schemaAdditionalProperties :: Maybe (OB.AdditionalProperties Schema)

    , _schemaEnum                 :: Maybe AN.EnumVal
    , _schemaType                 :: Maybe AN.TypeVal
    , _schemaAllOf                :: Maybe (NonEmpty Schema)
    , _schemaAnyOf                :: Maybe (NonEmpty Schema)
    , _schemaOneOf                :: Maybe (NonEmpty Schema)
    , _schemaNot                  :: Maybe Schema
    } deriving (Eq, Show)

emptySchema :: Schema
emptySchema = Schema
    { _schemaVersion              = Nothing
    , _schemaId                   = Nothing
    , _schemaRef                  = Nothing
    , _schemaDefinitions          = Nothing
    , _schemaOther                = mempty

    , _schemaMultipleOf           = Nothing
    , _schemaMaximum              = Nothing
    , _schemaExclusiveMaximum     = Nothing
    , _schemaMinimum              = Nothing
    , _schemaExclusiveMinimum     = Nothing

    , _schemaMaxLength            = Nothing
    , _schemaMinLength            = Nothing
    , _schemaPattern              = Nothing

    , _schemaMaxItems             = Nothing
    , _schemaMinItems             = Nothing
    , _schemaUniqueItems          = Nothing
    , _schemaItems                = Nothing
    , _schemaAdditionalItems      = Nothing

    , _schemaMaxProperties        = Nothing
    , _schemaMinProperties        = Nothing
    , _schemaRequired             = Nothing
    , _schemaDependencies         = Nothing
    , _schemaProperties           = Nothing
    , _schemaPatternProperties    = Nothing
    , _schemaAdditionalProperties = Nothing

    , _schemaEnum                 = Nothing
    , _schemaType                 = Nothing
    , _schemaAllOf                = Nothing
    , _schemaAnyOf                = Nothing
    , _schemaOneOf                = Nothing
    , _schemaNot                  = Nothing
    }

instance FromJSON Schema where
    parseJSON = withObject "Schema" $ \o -> do
        a  <- o .:! "$schema"
        b  <- o .:! "id"
        c  <- o .:! "$ref"
        d  <- o .:! "definitions"
        e  <- parseJSON (Object (HM.difference o internalSchemaHashMap))

        f  <- o .:! "multipleOf"
        g  <- o .:! "maximum"
        h  <- o .:! "exclusiveMaximum"
        i  <- o .:! "minimum"
        j  <- o .:! "exclusiveMinimum"

        k  <- o .:! "maxLength"
        l  <- o .:! "minLength"
        m  <- o .:! "pattern"

        n  <- o .:! "maxItems"
        o' <- o .:! "minItems"
        p  <- o .:! "uniqueItems"
        q  <- o .:! "items"
        r  <- o .:! "additionalItems"

        s  <- o .:! "maxProperties"
        t  <- o .:! "minProperties"
        u  <- o .:! "required"
        v  <- o .:! "dependencies"
        w  <- o .:! "properties"
        x  <- o .:! "patternProperties"
        y  <- o .:! "additionalProperties"

        z  <- o .:! "enum"
        a2 <- o .:! "type"
        b2 <- fmap _unNonEmpty' <$> o .:! "allOf"
        c2 <- fmap _unNonEmpty' <$> o .:! "anyOf"
        d2 <- fmap _unNonEmpty' <$> o .:! "oneOf"
        e2 <- o .:! "not"
        pure Schema
            { _schemaVersion              = a
            , _schemaId                   = b
            , _schemaRef                  = c
            , _schemaDefinitions          = d
            , _schemaOther                = e

            , _schemaMultipleOf           = f
            , _schemaMaximum              = g
            , _schemaExclusiveMaximum     = h
            , _schemaMinimum              = i
            , _schemaExclusiveMinimum     = j

            , _schemaMaxLength            = k
            , _schemaMinLength            = l
            , _schemaPattern              = m

            , _schemaMaxItems             = n
            , _schemaMinItems             = o'
            , _schemaUniqueItems          = p
            , _schemaItems                = q
            , _schemaAdditionalItems      = r

            , _schemaMaxProperties        = s
            , _schemaMinProperties        = t
            , _schemaRequired             = u
            , _schemaDependencies         = v
            , _schemaProperties           = w
            , _schemaPatternProperties    = x
            , _schemaAdditionalProperties = y

            , _schemaEnum                 = z
            , _schemaType                 = a2
            , _schemaAllOf                = b2
            , _schemaAnyOf                = c2
            , _schemaOneOf                = d2
            , _schemaNot                  = e2
            }

instance ToJSON Schema where
    -- | The way we resolve JSON Pointers to embedded schemas is by
    -- serializing the containing schema to a value and then resolving the
    -- pointer against it. This means that FromJSON and ToJSON must be
    -- isomorphic.
    --
    -- This influences the design choices in the library. E.g. right now
    -- there are two false values for "exclusiveMaximum" -- Nothing and
    -- Just False. We could have condensed them down by using () instead
    -- of Bool for "exclusiveMaximum". This would have made writing schemas
    -- in haskell easier, but we could no longer round trip through/from
    -- JSON without losing information.
    toJSON s = Object $ HM.union (mapMaybe ($ s) internalSchemaHashMap)
                                 (toJSON <$> _schemaOther s)
      where
        -- 'mapMaybe' is provided by unordered-containers after
        -- unordered-container-2.6.0.0, but until that is a little older
        -- (and has time to get into Stackage etc.) we use our own
        -- implementation.
        mapMaybe :: (v1 -> Maybe v2) -> HashMap k v1 -> HashMap k v2
        mapMaybe f = fmap fromJust . HM.filter isJust . fmap f

-- | Internal. Separate from ToJSON because it's also used
-- by FromJSON to determine what keys aren't official schema
-- keys and therefor should be included in _schemaOther.
internalSchemaHashMap :: HashMap Text (Schema -> Maybe Value)
internalSchemaHashMap = HM.fromList
    [ ("$schema"             , f _schemaVersion)
    , ("id"                  , f _schemaId)
    , ("$ref"                , f _schemaRef)
    , ("definitions"         , f _schemaDefinitions)

    , ("multipleOf"          , f _schemaMultipleOf)
    , ("maximum"             , f _schemaMaximum)
    , ("exclusiveMaximum"    , f _schemaExclusiveMaximum)
    , ("minimum"             , f _schemaMinimum)
    , ("exclusiveMinimum"    , f _schemaExclusiveMinimum)

    , ("maxLength"           , f _schemaMaxLength)
    , ("minLength"           , f _schemaMinLength)
    , ("pattern"             , f _schemaPattern)

    , ("maxItems"            , f _schemaMaxItems)
    , ("minItems"            , f _schemaMinItems)
    , ("uniqueItems"         , f _schemaUniqueItems)
    , ("items"               , f _schemaItems)
    , ("additionalItems"     , f _schemaAdditionalItems)

    , ("maxProperties"       , f _schemaMaxProperties)
    , ("minProperties"       , f _schemaMinProperties)
    , ("required"            , f _schemaRequired)
    , ("dependencies"        , f _schemaDependencies)
    , ("properties"          , f _schemaProperties)
    , ("patternProperties"   , f _schemaPatternProperties)
    , ("additionalProperties", f _schemaAdditionalProperties)

    , ("enum"                , f _schemaEnum)
    , ("type"                , f _schemaType)
    , ("allOf"               , f (fmap NonEmpty' . _schemaAllOf))
    , ("anyOf"               , f (fmap NonEmpty' . _schemaAnyOf))
    , ("oneOf"               , f (fmap NonEmpty' . _schemaOneOf))
    , ("not"                 , f _schemaNot)
    ]
  where
    f :: ToJSON a => (Schema -> Maybe a) -> Schema -> Maybe Value
    f = (fmap.fmap) toJSON

instance Arbitrary Schema where
    arbitrary = sized f
      where
        maybeGen :: Gen a -> Gen (Maybe a)
        maybeGen a = oneof [pure Nothing, Just <$> a]

        maybeRecurse :: Int -> Gen a -> Gen (Maybe a)
        maybeRecurse n a
            | n < 1     = pure Nothing
            | otherwise = maybeGen $ resize (n `div` 10) a

        f :: Int -> Gen Schema
        f n = do
            a  <- maybeGen arbitraryText
            b  <- maybeGen arbitraryText
            c  <- maybeGen arbitraryText
               -- NOTE: The next two fields are empty to generate cleaner schemas,
               -- but note that this means we don't test e.g. the invertability
               -- of these fields.
            d  <- pure Nothing -- _schemaDefinitions
            e  <- pure mempty -- _otherPairs

            f' <- maybeGen arbitraryPositiveScientific
            g  <- maybeGen arbitraryScientific
            h  <- arbitrary
            i  <- maybeGen arbitraryScientific
            j  <- arbitrary

            k  <- maybeGen (getPositive <$> arbitrary)
            l  <- maybeGen (getPositive <$> arbitrary)
            m  <- maybeGen arbitraryText

            n' <- maybeGen (getPositive <$> arbitrary)
            o  <- maybeGen (getPositive <$> arbitrary)
            p  <- arbitrary
            q  <- maybeRecurse n arbitrary
            r  <- maybeRecurse n arbitrary

            s  <- maybeGen (getPositive <$> arbitrary)
            t  <- maybeGen (getPositive <$> arbitrary)
            u  <- arbitrary
            v  <- maybeRecurse n arbitraryHashMap
            w  <- maybeRecurse n arbitraryHashMap
            x  <- maybeRecurse n arbitraryHashMap
            y  <- maybeRecurse n arbitrary

            z  <- arbitrary
            a2 <- arbitrary
            b2 <- maybeRecurse n (_unNonEmpty' <$> arbitrary)
            c2 <- maybeRecurse n (_unNonEmpty' <$> arbitrary)
            d2 <- maybeRecurse n (_unNonEmpty' <$> arbitrary)
            e2 <- maybeRecurse n arbitrary
            pure Schema
                { _schemaVersion              = a
                , _schemaId                   = b
                , _schemaRef                  = c
                , _schemaDefinitions          = d
                , _schemaOther                = e

                , _schemaMultipleOf           = f'
                , _schemaMaximum              = g
                , _schemaExclusiveMaximum     = h
                , _schemaMinimum              = i
                , _schemaExclusiveMinimum     = j

                , _schemaMaxLength            = k
                , _schemaMinLength            = l
                , _schemaPattern              = m

                , _schemaMaxItems             = n'
                , _schemaMinItems             = o
                , _schemaUniqueItems          = p
                , _schemaItems                = q
                , _schemaAdditionalItems      = r

                , _schemaMaxProperties        = s
                , _schemaMinProperties        = t
                , _schemaRequired             = u
                , _schemaDependencies         = v
                , _schemaProperties           = w
                , _schemaPatternProperties    = x
                , _schemaAdditionalProperties = y

                , _schemaEnum                 = z
                , _schemaType                 = a2
                , _schemaAllOf                = b2
                , _schemaAnyOf                = c2
                , _schemaOneOf                = d2
                , _schemaNot                  = e2
                }
