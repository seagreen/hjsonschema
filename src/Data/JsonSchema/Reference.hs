-- | TODO: The code handling resolution scope updates
-- is hacky and needs improvement.

module Data.JsonSchema.Reference where

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as H
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.HTTP.Conduit
import           Prelude              hiding (foldr)

combineIdAndRef :: Text -> Text -> Text
combineIdAndRef a b
  | "://" `T.isInfixOf` b              = b
  | T.length a < 1 || T.length b < 1   = a <> b
  | T.last a == '#' && T.head b == '#' = a <> T.tail b
  | otherwise                          = a <> b

combineIds :: Text -> Text -> Text
combineIds a b
  | b == "#" || b == ""                = a
  | "://" `T.isInfixOf` b              = b
  | T.length a < 1 || T.length b < 1   = a <> b
  | T.last a == '#' && T.head b == '#' = a <> T.tail b
  | otherwise                          = a <> b

newResolutionScope :: Text -> HashMap Text Value -> Text
newResolutionScope t o =
  case H.lookup "id" o of
    Just (String idKeyword) -> t `combineIds` idKeyword
    _                       -> t

refAndPointer :: Text -> Maybe (Text, Text)
refAndPointer val = getParts $ T.splitOn "#" val
  where
    getParts :: [Text] -> Maybe (Text, Text)
    getParts []    = Just ("","")
    getParts [x]   = Just (x,"")
    getParts [x,y] = Just (x,y)
    getParts _     = Nothing

fetchRef :: Text -> IO (Either Text (HashMap Text Value))
fetchRef t = do
  eResp <- safeGet t
  case eResp of
    Left e  -> return $ Left e
    Right b -> return . left T.pack $ eitherDecode b

safeGet :: Text -> IO (Either Text ByteString)
safeGet url =
  catch
    (return . Right =<< (simpleHttp . T.unpack) url)
    handler
  where
    handler :: SomeException -> IO (Either Text ByteString)
    handler e = return . Left . T.pack . show $ e
