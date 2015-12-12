
module Data.Validator.Reference where

import qualified Data.Aeson.Pointer     as P
import qualified Data.Text              as T
import           Data.Text.Encoding
import           Network.HTTP.Types.URI

import           Import

type URIBase = Maybe Text
type URIBaseAndFragment = (Maybe Text, Maybe Text)

newResolutionScope :: URIBase -> Maybe Text -> URIBase
newResolutionScope mScope idKeyword =
  case idKeyword of
    Just t -> fst . baseAndFragment $ resolveScopeAgainst mScope t
    _      -> mScope

resolveReference :: URIBase -> Text -> URIBaseAndFragment
resolveReference mScope t = baseAndFragment $ resolveScopeAgainst mScope t

isRemoteReference :: Text -> Bool
isRemoteReference uri = "://" `T.isInfixOf` uri

resolveFragment
  :: (FromJSON schema, ToJSON schema, Show schema)
  => Maybe Text
  -> schema
  -> Maybe schema
resolveFragment Nothing schema        = Just schema
resolveFragment (Just pointer) schema = do
  let urlDecoded = decodeUtf8 . urlDecode True . encodeUtf8 $ pointer
  p <- either (const Nothing) Just (P.unescape urlDecoded)
  x <- either (const Nothing) Just (P.resolve p (toJSON schema))
  case fromJSON x of
    Error _         -> Nothing
    Success schema' -> Just schema'

--------------------------------------------------
-- * Internal
--------------------------------------------------

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
    smartAppend = case baseAndFragment scope of
                    (Just base,_) -> base <> t
                    _             -> t
