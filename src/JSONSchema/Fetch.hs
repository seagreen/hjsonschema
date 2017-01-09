
module JSONSchema.Fetch where

import           Import

import           Control.Arrow                  (left)
import           Control.Exception              (IOException, catch)
import           Control.Monad                  (foldM)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T
import qualified Network.HTTP.Client            as NC

import           JSONSchema.Validator.Reference (resolveReference,
                                                 updateResolutionScope)

--------------------------------------------------
-- * Types
--------------------------------------------------

-- | This is all the fetching functions need to know about a particular
-- JSON Schema draft, e.g. JSON Schema Draft 4.
data FetchInfo schema = FetchInfo
    { _fiEmbedded :: schema -> ([schema], [schema])
    , _fiId       :: schema -> Maybe Text
    , _fiRef      :: schema -> Maybe Text
    }

data ReferencedSchemas schema = ReferencedSchemas
    { _rsStarting  :: !schema
      -- ^ Used to resolve relative references when we don't know what the scope
      -- of the current schema is. This only happens with starting schemas
      -- because if we're using a remote schema we had to know its URI in order
      -- to fetch it.
      --
      -- Tracking the starting schema (instead of just resolving the reference to
      -- the current schema being used for validation) is necessary for cases
      -- where schemas are embedded inside one another. For instance in this
      -- case not distinguishing the starting and "foo" schemas sends the code
      -- into an infinite loop:
      --
      -- {
      --   "additionalProperties": false,
      --   "properties": {
      --     "foo": {
      --       "$ref": "#"
      --     }
      --   }
      -- }
    , _rsSchemaMap :: !(HashMap Text schema)
      -- ^ Map of URIs to schemas.
    } deriving (Eq, Show)

-- | Keys are URIs (without URI fragments).
newtype URISchemaMap schema
    = URISchemaMap { _unURISchemaMap :: HashMap Text schema }
    deriving (Eq, Show)

data SchemaWithURI schema = SchemaWithURI
    { _swSchema :: !schema
    , _swURI    :: !(Maybe Text)
      -- ^ This is the URI identifying the document containing the schema.
      -- It's different than the schema's "id" field, which controls scope
      -- when resolving references contained in the schema.

      -- TODO: Make the no URI fragment requirement unnecessary.
    } deriving (Eq, Show)

getReference :: ReferencedSchemas schema -> Maybe Text -> Maybe schema
getReference referenced Nothing  = Just (_rsStarting referenced)
getReference referenced (Just t) = HM.lookup t (_rsSchemaMap referenced)

--------------------------------------------------
-- * Fetch via HTTP
--------------------------------------------------

data HTTPFailure
    = HTTPParseFailure   Text
    | HTTPRequestFailure NC.HttpException
    deriving Show

-- | Take a schema. Retrieve every document either it or its subschemas
-- include via the "$ref" keyword.
referencesViaHTTP'
    :: forall schema. FromJSON schema
    => FetchInfo schema
    -> SchemaWithURI schema
    -> IO (Either HTTPFailure (URISchemaMap schema))
referencesViaHTTP' info sw = do
    manager <- NC.newManager NC.defaultManagerSettings
    let f = referencesMethodAgnostic (getURL manager) info sw
    catch (left HTTPParseFailure <$> f) handler
  where
    getURL :: NC.Manager -> Text -> IO BS.ByteString
    getURL man url = do
        request <- NC.parseUrlThrow (T.unpack url)
        LBS.toStrict . NC.responseBody <$> NC.httpLbs request man

    handler
        :: NC.HttpException
        -> IO (Either HTTPFailure (URISchemaMap schema))
    handler = pure . Left . HTTPRequestFailure

--------------------------------------------------
-- * Fetch via Filesystem
--------------------------------------------------

data FilesystemFailure
    = FSParseFailure Text
    | FSReadFailure  IOException
    deriving (Show, Eq)

referencesViaFilesystem'
    :: forall schema. FromJSON schema
    => FetchInfo schema
    -> SchemaWithURI schema
    -> IO (Either FilesystemFailure (URISchemaMap schema))
referencesViaFilesystem' info sw = catch (left FSParseFailure <$> f) handler
  where
    f :: IO (Either Text (URISchemaMap schema))
    f = referencesMethodAgnostic (BS.readFile . T.unpack) info sw

    handler
        :: IOException
        -> IO (Either FilesystemFailure (URISchemaMap schema))
    handler = pure . Left . FSReadFailure

--------------------------------------------------
-- * Method Agnostic Fetching Tools
--------------------------------------------------

-- | A version of 'fetchReferencedSchema's where the function to fetch
-- schemas is provided by the user. This allows restrictions to be added,
-- e.g. rejecting non-local URIs.
referencesMethodAgnostic
    :: forall schema. FromJSON schema
    => (Text -> IO BS.ByteString)
    -> FetchInfo schema
    -> SchemaWithURI schema
    -> IO (Either Text (URISchemaMap schema))
referencesMethodAgnostic fetchRef info =
    getRecursiveReferences fetchRef info (URISchemaMap mempty)

getRecursiveReferences
    :: forall schema. FromJSON schema
    => (Text -> IO BS.ByteString)
    -> FetchInfo schema
    -> URISchemaMap schema
    -> SchemaWithURI schema
    -> IO (Either Text (URISchemaMap schema))
getRecursiveReferences fetchRef info referenced sw =
    foldM f (Right referenced) (includeSubschemas info sw)
  where
    f :: Either Text (URISchemaMap schema)
      -> SchemaWithURI schema
      -> IO (Either Text (URISchemaMap schema))
    f (Left e) _                            = pure (Left e)
    f (Right (URISchemaMap usm)) (SchemaWithURI schema mUri) =
        case newRef of
            Nothing  -> pure (Right (URISchemaMap usm))
            Just uri -> do
                bts <- fetchRef uri
                case eitherDecodeStrict bts of
                    Left e     -> pure . Left . T.pack $ e
                    Right schm -> getRecursiveReferences
                                      fetchRef
                                      info
                                      (URISchemaMap (HM.insert uri schm usm))
                                      (SchemaWithURI schm (Just uri))
      where
        newRef :: Maybe Text
        newRef
          | Just (Just uri,_) <- resolveReference mUri <$> _fiRef info schema
              = case HM.lookup uri usm of
                    Nothing -> Just uri
                    Just _  -> Nothing
          | otherwise = Nothing

-- | Return the schema passed in as an argument, as well as every
-- subschema contained within it.
includeSubschemas
    :: forall schema.
       FetchInfo schema
    -> SchemaWithURI schema
    -> [SchemaWithURI schema]
includeSubschemas info (SchemaWithURI schema mUri) =
    SchemaWithURI schema mUri
    : (includeSubschemas info =<< subSchemas)
  where
    subSchemas :: [SchemaWithURI schema]
    subSchemas =
      (\a -> SchemaWithURI a (updateResolutionScope mUri (_fiId info schema)))
         <$> uncurry (<>) (_fiEmbedded info schema)
