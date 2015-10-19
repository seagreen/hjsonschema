
module Data.JsonSchema.Reference where

import qualified Data.HashMap.Strict    as H
import           Data.JsonPointer
import qualified Data.Text              as T
import           Data.Text.Encoding
import           Network.HTTP.Types.URI

import           Import

type URIBase = Maybe Text
type URIBaseAndFragment = (Maybe Text, Maybe Text)

newResolutionScope :: URIBase -> HashMap Text Value -> URIBase
newResolutionScope mScope o =
  case H.lookup "id" o of
    Just (String t) -> fst . baseAndFragment $ resolveScopeAgainst mScope t
    _               -> mScope

resolveReference :: URIBase -> Text -> URIBaseAndFragment
resolveReference mScope t = baseAndFragment $ resolveScopeAgainst mScope t

isRemoteReference :: Text -> Bool
isRemoteReference uri = "://" `T.isInfixOf` uri

resolveFragment :: Maybe Text -> HashMap Text Value -> HashMap Text Value
resolveFragment Nothing hm        = hm
resolveFragment (Just pointer) hm =
  let urlDecoded = decodeUtf8 . urlDecode True . encodeUtf8 $ pointer
  in case jsonPointer urlDecoded of
    Left _  -> hm
    Right p ->
      case resolvePointer p (Object hm) of
        Right (Object hm') -> hm'
        _                  -> hm

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
