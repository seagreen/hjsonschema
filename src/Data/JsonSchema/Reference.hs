module Data.JsonSchema.Reference where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as H
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.Wreq
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
    Left err -> return (Left err)
    Right b  ->
      case eitherDecode b of
        Right (Object z) -> return (Right z)
        Right v          -> return . Left $
          "fetchRef returned the following instead of an object" <> T.pack (show v)
        Left e           -> return . Left . T.pack $ e

safeGet :: Text -> IO (Either Text ByteString)
safeGet url =
  catch
    (return . Right . (^. responseBody) =<< get (T.unpack url))
    handler
  where
    handler :: SomeException -> IO (Either Text ByteString)
    handler e = return . Left . T.pack . show $ e
