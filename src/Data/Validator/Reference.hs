-- | JSON Reference is described here:
-- <http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03>
--
-- And is extended for JSON Schema here:
-- <http://json-schema.org/latest/json-schema-core.html#anchor26>

module Data.Validator.Reference where

import           Import

import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified JSONPointer            as JP
import           Network.HTTP.Types.URI (urlDecode)
import           System.FilePath        ((</>), dropFileName)

-- | TODO: Replace these with actual URI data types.
type URIBase = Maybe Text
type URIBaseAndFragment = (Maybe Text, Maybe Text)

updateResolutionScope :: URIBase -> Maybe Text -> URIBase
updateResolutionScope mScope idKeyword
    | Just t <- idKeyword = fst . baseAndFragment $ resolveScopeAgainst mScope t
    | otherwise           = mScope

resolveReference :: URIBase -> Text -> URIBaseAndFragment
resolveReference mScope t = baseAndFragment $ resolveScopeAgainst mScope t

resolveFragment
    :: (FromJSON schema, ToJSON schema)
    => Maybe Text
    -> schema
    -> Maybe schema
resolveFragment Nothing schema        = Just schema
resolveFragment (Just pointer) schema = do
    let urlDecoded = decodeUtf8 . urlDecode True . encodeUtf8 $ pointer
    p <- either (const Nothing) Just (JP.unescape urlDecoded)
    x <- either (const Nothing) Just (JP.resolve p (toJSON schema))
    case fromJSON x of
        Error _         -> Nothing
        Success schema' -> Just schema'

--------------------------------------------------
-- * Helpers
--------------------------------------------------

isRemoteReference :: Text -> Bool
isRemoteReference = T.isInfixOf "://"

baseAndFragment :: Text -> URIBaseAndFragment
baseAndFragment = f . T.splitOn "#"
  where
    f :: [Text] -> URIBaseAndFragment
    f [x]   = (g x, Nothing)
    f [x,y] = (g x, g y)
    f _     = (Nothing, Nothing)

    g "" = Nothing
    g x  = Just x

resolveScopeAgainst :: Maybe Text -> Text -> Text
resolveScopeAgainst Nothing t = t
resolveScopeAgainst (Just scope) t
    | isRemoteReference t = t
    | otherwise           = smartAppend
  where
    -- There shouldn't be a fragment at the end of a scope URI,
    -- but just in case a user leaves one in we want to be sure
    -- to cut it off before appending.
    smartAppend :: Text
    smartAppend =
        case baseAndFragment scope of
            (Just base,_) ->
                case T.unpack t of
                    -- We want "/foo" and "#/bar" to combine into
                    -- "/foo#/bar" not "/foo/#/bar".
                    '#':_ -> base <> t
                    _     -> T.pack (dropFileName (T.unpack base) </> T.unpack t)
            _ -> t
