-- | JSON Reference is described here:
-- <http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03>
--
-- And is extended for JSON Schema here:
-- <http://json-schema.org/latest/json-schema-core.html#anchor26>
module JSONSchema.Validator.Reference where

import           Import

import qualified Data.Text as T
import           System.FilePath (dropFileName, (</>))

data Scope schema = Scope
    { _topLevelDocument :: schema
    , _documentURI      :: Maybe Text
    , _currentBaseURI   :: BaseURI
    } deriving (Eq, Show)

newtype BaseURI
    = BaseURI { _unBaseURI :: Maybe Text }
    deriving (Eq, Show)

-- | TODO: no `type`s.
type URIAndFragment = (Maybe Text, Maybe Text)

updateResolutionScope :: BaseURI -> Maybe Text -> BaseURI
updateResolutionScope base idKeyword =
    case idKeyword of
        Just t  -> BaseURI . fst . baseAndFragment $ resolveScopeAgainst base t
        Nothing -> base

resolveReference :: BaseURI -> Text -> URIAndFragment
resolveReference base t = baseAndFragment (resolveScopeAgainst base t)

--------------------------------------------------
-- * Helpers
--------------------------------------------------

isRemoteReference :: Text -> Bool
isRemoteReference = T.isInfixOf "://"

baseAndFragment :: Text -> URIAndFragment
baseAndFragment = f . T.splitOn "#"
  where
    f :: [Text] -> URIAndFragment
    f [x]   = (g x, Nothing)
    f [x,y] = (g x, g y)
    f _     = (Nothing, Nothing)

    g "" = Nothing
    g x  = Just x

resolveScopeAgainst :: BaseURI -> Text -> Text
resolveScopeAgainst (BaseURI Nothing) t = t
resolveScopeAgainst (BaseURI (Just base)) t
    | isRemoteReference t = t
    | otherwise           = smartAppend
  where
    -- There shouldn't be a fragment at the end of a scope URI,
    -- but just in case a user leaves one in we want to be sure
    -- to cut it off before appending.
    smartAppend :: Text
    smartAppend =
        case baseAndFragment base of
            (Just uri,_) ->
                case T.unpack t of
                    -- We want "/foo" and "#/bar" to combine into
                    -- "/foo#/bar" not "/foo/#/bar".
                    '#':_ -> base <> t
                    _     -> T.pack (dropFileName (T.unpack uri) </> T.unpack t)
            _ -> t
