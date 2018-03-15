module JSONSchema.Fetch where

import           Import

import           Control.Exception (IOException, catch)
import           Control.Monad (foldM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Network.HTTP.Client as NC
import qualified Network.HTTP.Client.TLS as NCTLS

import           JSONSchema.Validator.Reference (BaseURI(..), resolveReference,
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

-- | Keys are URIs (without URI fragments).
newtype URISchemaMap schema
    = URISchemaMap { _unURISchemaMap :: HashMap Text schema }
    deriving (Eq, Show, Monoid)

-- | A top-level schema along with its location.
data SchemaWithURI schema = SchemaWithURI
    { _swSchema :: !schema
    , _swURI    :: !(Maybe Text)
      -- ^ This is the URI from which the document was originally fetched.
      -- It's different than the schema's "id" field, which controls scope
      -- when resolving references contained in the schema.
    } deriving (Eq, Show)

getReference :: URISchemaMap schema -> Text -> Maybe schema
getReference schemaMap t = HM.lookup t (_unURISchemaMap schemaMap)

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
    manager <- NC.newManager NCTLS.tlsManagerSettings
    let f = referencesMethodAgnostic (getURL manager) info sw
    catch (first HTTPParseFailure <$> f) handler
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
referencesViaFilesystem' info sw = catch (first FSParseFailure <$> f) handler
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
    f (Left e) _ = pure (Left e)
    f (Right (URISchemaMap schemaMap)) (SchemaWithURI schema mURI) =
        case newRef of
            Nothing  -> pure (Right (URISchemaMap schemaMap))
            Just uri -> do
                bts <- fetchRef uri
                case eitherDecodeStrict bts of
                    Left e  -> pure . Left . T.pack $ e
                    Right s -> getRecursiveReferences
                                   fetchRef
                                   info
                                   (URISchemaMap (HM.insert uri s schemaMap))
                                   (SchemaWithURI s (Just uri))
      where
        newRef :: Maybe Text
        newRef = do
            ref <- _fiRef info schema

            -- Consider the reference before updating the scope.
            -- If it's a only a fragment this isn't referencing
            -- a new document.
            void (fst (resolveReference (BaseURI Nothing) ref))

            uri <- fst (resolveReference (BaseURI mURI) ref)
            case HM.lookup uri schemaMap of
                Nothing -> Just uri
                Just _  -> Nothing

-- | Return the schema passed in as an argument, as well as every
-- subschema contained within it.
includeSubschemas
    :: forall schema.
       FetchInfo schema
    -> SchemaWithURI schema
    -> [SchemaWithURI schema]
includeSubschemas info (SchemaWithURI schema mURI) =
    SchemaWithURI schema mURI
    : (includeSubschemas info =<< subSchemas)
  where
    subSchemas :: [SchemaWithURI schema]
    subSchemas =
        let newScope = updateResolutionScope (BaseURI mURI) (_fiId info schema)
            updateScope s = SchemaWithURI s (_unBaseURI newScope)
        in updateScope <$> uncurry (<>) (_fiEmbedded info schema)
