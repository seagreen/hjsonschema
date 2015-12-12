{-# LANGUAGE ScopedTypeVariables #-}

module Data.JsonSchema.Fetch where

import           Control.Exception        (SomeException(..), catch)
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.HashMap.Strict      as H
import qualified Data.Text                as T
import           Network.HTTP.Client

import           Data.Validator.Reference (isRemoteReference,
                                           newResolutionScope,
                                           resolveReference)
import           Import

-- For GHCs before 7.10:
import           Prelude                  hiding (concat, sequence)

data SchemaContext schema = SchemaContext
  { _scURI    :: !(Maybe Text)
  -- ^ Must not include a URI fragment, e.g. use
  -- "http://example.com/foo" not "http://example.com/foo#bar".
  --
  -- This is the URI identifying the document containing the schema.
  -- It's different than the schema's "id" field, which controls scope
  -- when resolving references contained in the schema.

  -- TODO: Make the no URI fragment requirement unnecessary.

  , _scSchema :: !schema
  } deriving (Eq, Show)

-- | Keys are URIs (without URI fragments).
type URISchemaMap schema = HashMap Text schema

data SchemaCache schema = SchemaCache
  { _startingSchema :: !schema
  -- ^ Used to resolve relative references.
  , _cachedSchemas  :: !(URISchemaMap schema)
  } deriving (Eq, Show)

-- | Take a schema. Retrieve every document either it or its subschemas
-- include via the "$ref" keyword. Load a 'SchemaCache' out with them.
fetchReferencedSchemas
  :: forall schema. FromJSON schema
  => (schema -> [schema])
  -> (schema -> Maybe Text)
  -> (schema -> Maybe Text)
  -> URISchemaMap schema
  -> SchemaContext schema
  -> IO (Either Text (SchemaCache schema))
fetchReferencedSchemas embedded getId getRef cache sc = do
  manager <- newManager defaultManagerSettings
  catch (Right <$> f manager) handler
  where
    f manager = fetchReferencedSchemas' embedded getId getRef
                                        (simpleGET manager) cache sc

    handler :: SomeException -> IO (Either Text (SchemaCache schema))
    handler e = pure . Left . T.pack . show $ e

-- | Based on 'Network.Http.Conduit.simpleHttp' from http-conduit.
simpleGET :: Manager -> Text -> IO LBS.ByteString
simpleGET man url = do
  req <- parseUrl (T.unpack url)
  responseBody <$> httpLbs req { requestHeaders = ("Connection", "close")
                               : requestHeaders req
                               } man

-- | A version of 'fetchReferencedSchema's where the function to fetch
-- schemas is provided by the user. This allows restrictions to be added,
-- e.g. rejecting non-local URIs.
fetchReferencedSchemas'
  :: forall schema. FromJSON schema
  => (schema -> [schema])
  -> (schema -> Maybe Text)
  -> (schema -> Maybe Text)
  -> (Text -> IO LBS.ByteString)
  -> URISchemaMap schema
  -> SchemaContext schema
  -> IO (SchemaCache schema)
fetchReferencedSchemas' embedded getId getRef fetchRef cache sc = do
  let startingCache = case _scURI sc of
                        Nothing  -> cache
                        Just uri -> H.insert uri (_scSchema sc) cache
  SchemaCache (_scSchema sc) <$> foldlM fetchRecursively
                                        startingCache
                                        (includeSubschemas embedded getId sc)
  where
    fetchRecursively
      :: URISchemaMap schema
      -> SchemaContext schema
      -> IO (URISchemaMap schema)
    fetchRecursively g (SchemaContext mUri schema) = do
      -- Resolving the new scope is necessary here because of situations
      -- like this:
      --
      -- {
      --     "id": "http://localhost:1234/",
      --     "items": {
      --         "id": "folder/",
      --         "items": {"$ref": "folderInteger.json"}
      --     }
      -- }
      let scope = newResolutionScope mUri (getId schema)
      case resolveReference scope <$> getRef schema of
        Just (Just uri,_) ->
          if not (isRemoteReference uri) || H.member uri g
            then pure g
            else do
              bts <- fetchRef uri
              case eitherDecode bts of
                Left e     -> ioError (userError e)
                Right schm -> _cachedSchemas <$>
                  fetchReferencedSchemas' embedded getId getRef fetchRef
                                          g (SchemaContext (Just uri) schm)
        _ -> pure g

-- | Return the schema passed in as an argument, as well as every
-- subschema contained within it.
includeSubschemas
  :: forall schema.
     (schema -> [schema])
  -> (schema -> Maybe Text)
  -> SchemaContext schema
  -> [SchemaContext schema]
includeSubschemas embedded getId (SchemaContext mUri schema) =
  SchemaContext mUri schema
  : (includeSubschemas embedded getId =<< subSchemas)
  where
    subSchemas :: [SchemaContext schema]
    subSchemas = SchemaContext (newResolutionScope mUri (getId schema))
             <$> embedded schema
