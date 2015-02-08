-- | There should be a JSON Reference library for haskell.

module Data.JsonSchema.JsonReference where

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
import qualified Data.Vector          as V
import           Network.Wreq
import           Prelude              hiding (foldr)
import           Text.Read            (readMaybe)

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

updateId :: Text -> HashMap Text Value -> Text
updateId t o =
  case H.lookup "id" o of
    Just (String idVal) -> t `combineIds` idVal
    _                   -> t

refAndP :: Text -> Maybe (Text, Text)
refAndP val = getParts $ T.splitOn "#" val
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
    Left _  -> return (Left "TODO")
    Right b ->
      case decode b of
        Just (Object z) -> return (Right z)
        _               -> return (Left "TODO")

safeGet :: Text -> IO (Either Text ByteString)
safeGet url =
  catch
    (return . Right . (^. responseBody) =<< get (T.unpack url))
    handler
  where
    handler :: SomeException -> IO (Either Text ByteString)
    handler e = return . Left . T.pack . show $ e

-- | There should be a JSON Pointer library.
--
-- Spec: http://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-07
jsonPointer :: Text -> Value -> Maybe Value
jsonPointer pntr = resolve (T.splitOn "/" pntr)
  where
    resolve :: [Text] -> Value -> Maybe Value
    resolve (referenceToken:ts) a =
      let t = unescape referenceToken
      in case T.length t of
        0 -> resolve ts a
        _ ->
          case a of
            (Object b) -> H.lookup t b >>= resolve ts
            (Array c)  -> do
              n <- readMaybe (T.unpack t)
              when (n < 0 || n + 1 > V.length c) Nothing
              resolve ts (c V.! n)
            _ -> Nothing
    resolve _ a = Just a

    -- TODO: do more things need to be escaped?
    unescape :: Text -> Text
    unescape t = T.replace "%25" "%" $ T.replace "~0" "~" $ T.replace "~1" "/" t
