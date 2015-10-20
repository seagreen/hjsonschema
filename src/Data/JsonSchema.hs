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
import qualified Data.Text as T
import qualified Data.Vector as V

import           Data.JsonSchema.Core
import           Data.JsonSchema.Draft4
import qualified Data.JsonSchema.Helpers as HE
import           Data.JsonSchema.Reference
import           Import

-- For GHCs before 7.10:
import           Prelude hiding (sequence)

-- | Take a schema. Retrieve every document either it or its
-- subschemas include via the "$ref" keyword. Load a 'Graph' out
-- with them.
--
-- TODO: This function's URL processing is hacky and needs improvement.
fetchReferencedSchemas :: Spec err -> RawSchema -> Graph -> IO (Either Text Graph)
fetchReferencedSchemas = fetchReferencedSchemas' HE.defaultFetch

-- | A version of fetchReferencedSchemas where the function to make requests
-- is provided by the user. This allows restrictions to be added, e.g. rejecting
-- non-local URLs.
fetchReferencedSchemas'
  -- The `Functor m` declaration is for GHCs before 7.10.
  :: forall m e t err. (MonadIO m, Functor m, MonadError t e, Traversable e, IsString t)
  => (Text -> m (e LBS.ByteString))
  -> Spec err
  -> RawSchema
  -> Graph
  -> m (e Graph)
fetchReferencedSchemas' fetchRef spec rawSchema graph =
  let startingGraph = H.insert (_rsURI rawSchema) (_rsObject rawSchema) graph
  in foldlM (modFetch fetch) (return startingGraph) (includeSubschemas rawSchema)

  where
    includeSubschemas :: RawSchema -> Vector RawSchema
    includeSubschemas r =
      let newId = newResolutionScope (_rsURI r) (_rsObject r)
          xs = H.intersectionWith (\(ValSpec f _) x -> f newId x) (_unSpec spec) (_rsObject r)
          ys = V.concat . H.elems $ xs -- TODO: optimize
      in V.cons r . V.concat . V.toList $ includeSubschemas <$> ys

    modFetch :: (Graph -> RawSchema -> m (e Graph)) -> e Graph -> RawSchema -> m (e Graph)
    modFetch f eg rs = join <$> sequence (flip f rs <$> eg)

    fetch :: Graph -> RawSchema -> m (e Graph)
    fetch g r =
      case H.lookup "$ref" (_rsObject r) >>= HE.toTxt >>= refAndPointer of
        Nothing     -> return (return g)
        Just (s, _) ->
          let url = (_rsURI r `combineIdAndRef` s)
          in if T.length url <= 0 || H.member url g || not ("://" `T.isInfixOf` url)
            then return (return g)
            else modDec (decodeResponse url) =<< fetchRef url
      where
        decodeResponse :: Text -> LBS.ByteString -> m (e Graph)
        decodeResponse url bts =
          case eitherDecode bts of
            Left e    -> return $ throwError (fromString e)
            Right obj -> fetchReferencedSchemas' fetchRef spec (RawSchema url obj) g

        modDec :: (LBS.ByteString -> m (e Graph)) -> e LBS.ByteString -> m (e Graph)
        modDec f eBts = join <$> sequence (f <$> eBts)
