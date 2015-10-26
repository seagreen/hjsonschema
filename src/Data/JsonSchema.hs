{-# LANGUAGE ScopedTypeVariables #-}

module Data.JsonSchema
  ( module Data.JsonSchema.Core
    -- * Schema feching tools
  , module Data.JsonSchema
    -- * Draft 4 specific
  , module Data.JsonSchema.Draft4
  ) where

import           Control.Monad.Except (MonadError, MonadIO, join, throwError)
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable
import qualified Data.HashMap.Strict as H
import           Data.String

import           Data.JsonSchema.Core
import           Data.JsonSchema.Draft4
import qualified Data.JsonSchema.Helpers as HE
import           Data.JsonSchema.Reference
import           Import

-- For GHCs before 7.10:
import           Prelude hiding (concat, sequence)

-- | Take a schema. Retrieve every document either it or its
-- subschemas include via the "$ref" keyword. Load a 'SchemaGraph' out
-- with them.
fetchReferencedSchemas :: Spec err -> SchemaCache -> RawSchema -> IO (Either Text SchemaGraph)
fetchReferencedSchemas = fetchReferencedSchemas' HE.defaultFetch

-- | A version of fetchReferencedSchemas where the function to make requests
-- is provided by the user. This allows restrictions to be added, e.g. rejecting
-- non-local URIs.
fetchReferencedSchemas'
  -- The `Functor m` declaration is for GHCs before 7.10.
  :: forall m e t err. (MonadIO m, Functor m, MonadError t e, Traversable e, IsString t)
  => (Text -> m (e LBS.ByteString))
  -> Spec err
  -> SchemaCache
  -> RawSchema
  -> m (e SchemaGraph)
fetchReferencedSchemas' fetchRef spec cache rawSchema = do
  let startingCache = case _rsURI rawSchema of
                        Nothing  -> cache
                        Just uri -> H.insert uri (_rsData rawSchema) cache
  fmap (SchemaGraph rawSchema) <$> foldlM fetch (return startingCache) (includeSubschemas rawSchema)

  where
    -- TODO: use a fold here
    includeSubschemas :: RawSchema -> [RawSchema]
    includeSubschemas r =
      let scope = _rsURI r `newResolutionScope` _rsData r
          xs = H.intersectionWith (\(ValSpec f _) x -> f scope x) (_unSpec spec) (_rsData r)
      in r : (concat . concat . H.elems $ fmap includeSubschemas <$> xs)

    fetch :: e SchemaCache -> RawSchema -> m (e SchemaCache)
    fetch eg r = join <$> sequence (run <$> eg)
      where
        run :: SchemaCache -> m (e SchemaCache)
        run g = do
          -- Resolving the new scope is necessary here because of situations like this:
          --
          -- {
          --     "id": "http://localhost:1234/",
          --     "items": {
          --         "id": "folder/",
          --         "items": {"$ref": "folderInteger.json"}
          --     }
          -- }
          let scope = newResolutionScope (_rsURI r) (_rsData r)
          case resolveReference scope <$> (H.lookup "$ref" (_rsData r) >>= HE.toTxt) of
            Just (Just uri,_) ->
              if not (isRemoteReference uri) || H.member uri g
                then return (return g)
                else fetchRef uri >>= decodeResponse g uri
            _ -> return (return g)

        decodeResponse :: SchemaCache -> Text -> e LBS.ByteString -> m (e SchemaCache)
        decodeResponse g uri eBts = join <$> sequence (runDecode g uri <$> eBts)

        runDecode :: SchemaCache -> Text -> LBS.ByteString -> m (e SchemaCache)
        runDecode g uri bts =
          case eitherDecode bts of
            Left e    -> return $ throwError (fromString e)
            Right obj -> fmap _cachedSchemas <$> fetchReferencedSchemas' fetchRef spec g (RawSchema (Just uri) obj)
