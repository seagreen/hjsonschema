
module Data.JsonSchema.Helpers where

import           Control.Exception
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as H
import           Data.Scientific
import qualified Data.Set as S
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Network.HTTP.Client

import           Data.JsonSchema.Core
import           Import

--------------------------------------------------
-- * Embedded schemas finders
--------------------------------------------------

noEm :: EmbeddedSchemas
noEm _ _ = mempty

objEmbed :: EmbeddedSchemas
objEmbed t (Object o) = pure (RawSchema t o)
objEmbed _ _ = mempty

arrayEmbed :: EmbeddedSchemas
arrayEmbed t (Array vs) = objEmbed t =<< V.toList vs
arrayEmbed _ _ = mempty

objOrArrayEmbed :: EmbeddedSchemas
objOrArrayEmbed t v@(Object _) = objEmbed t v
objOrArrayEmbed t v@(Array _) = arrayEmbed t v
objOrArrayEmbed _ _ = mempty

objMembersEmbed :: EmbeddedSchemas
objMembersEmbed t (Object o) = objEmbed t =<< H.elems o
objMembersEmbed _ _ = mempty

--------------------------------------------------
-- * Modify Validators for use in Specs
--------------------------------------------------

giveName
  :: forall err. err
  -> ValidatorConstructor err [FailureInfo]
  -> ValidatorConstructor err [ValidationFailure err]
giveName err f spec g rs v _ = (fmap.fmap) (ValidationFailure err) <$> f spec g rs v mempty

modifyName
  :: forall valErr schemaErr. (valErr -> schemaErr)
  -> ValidatorConstructor schemaErr [ValidationFailure valErr]
  -> ValidatorConstructor schemaErr [ValidationFailure schemaErr]
modifyName failureHandler f spec g rs v _ = (fmap.fmap) modErr <$> f spec g rs v mempty
  where
    modErr :: ValidationFailure valErr -> ValidationFailure schemaErr
    modErr (ValidationFailure a b) = ValidationFailure (failureHandler a) b

-- | It's important to know if an object's a validator (even if it will never run,
-- like the definitions validator) because parts of it might be referenced by other
-- validators. If one of those referenced parts is itself a valid reference we
-- need to have fetched the correct value for it. So validators that won't run are
-- different than non-validator objects, because even if a non-validator object has
-- a $ref" keyword it's not a valid reference and shouldn't be fetched.
neverBuild :: ValidatorConstructor err [ValidationFailure err]
neverBuild _ _ _ _ _ = Nothing

--------------------------------------------------
-- * Utils
--------------------------------------------------

-- | Export the fetch function used by fetchReferencedSchemas
defaultFetch :: Text -> IO (Either Text LBS.ByteString)
defaultFetch url = do
  eResp <- catch (Right <$> simpleHttp') handler
  case eResp of
    Left e  -> return $ Left e
    Right b -> return $ Right b

    where
      handler :: SomeException -> IO (Either Text LBS.ByteString)
      handler e = return . Left . T.pack . show $ e

      -- Modeled on Network.Http.Conduit.simpleHttp from http-conduit.
      -- simpleHttp also sets "Connection: close"
      simpleHttp' :: IO LBS.ByteString
      simpleHttp' = do
        man <- newManager defaultManagerSettings
        req <- parseUrl (T.unpack url)
        responseBody <$> httpLbs req { requestHeaders = ("Connection", "close") : requestHeaders req } man

modifyFailureName :: (a -> b) -> ValidationFailure a -> ValidationFailure b
modifyFailureName f (ValidationFailure a b) = ValidationFailure (f a) b

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a

runMaybeVal :: Maybe (Value -> [a]) -> Value -> [a]
runMaybeVal Nothing _ = mempty
runMaybeVal (Just val) x = val x

runMaybeVal'
  :: Maybe (Value -> ([a], Value))
  -> Value
  -> ([a], Value)
runMaybeVal' Nothing x = (mempty, x)
runMaybeVal' (Just val) x = val x

toObj :: Value -> Maybe (HashMap Text Value)
toObj (Object a) = Just a
toObj _ = Nothing

fromJSONInt :: Value -> Maybe Int
fromJSONInt (Number n) = toBoundedInteger n
fromJSONInt _ = Nothing

toTxt :: Value -> Maybe Text
toTxt (String t) = Just t
toTxt _ = Nothing

greaterThanZero :: (Num a, Ord a) => a -> Maybe ()
greaterThanZero n = if n <= 0 then Nothing else Just ()

tshow :: Show a => a -> Text
tshow = T.pack . show

allUnique :: (Ord a) => Vector a -> Bool
allUnique xs = S.size (S.fromList (V.toList xs)) == V.length xs

-- | This needs benchmarking, but it can't be as bad as using O(n^2) nub.
-- (We can't use our allUnique function directly on Values because they're
-- not an instance of Ord).
allUniqueValues :: Vector Value -> Bool
allUniqueValues = allUnique . fmap OrdValue

newtype OrdValue = OrdValue Value deriving Eq

instance Ord OrdValue where
  (OrdValue Null) `compare` (OrdValue Null) = EQ
  (OrdValue Null) `compare` _               = LT
  _               `compare` (OrdValue Null) = GT

  (OrdValue (Bool x)) `compare` (OrdValue (Bool y)) = x `compare` y
  (OrdValue (Bool _)) `compare` _                   = LT
  _                   `compare` (OrdValue (Bool _)) = GT

  (OrdValue (Number x)) `compare` (OrdValue (Number y)) = x `compare` y
  (OrdValue (Number _)) `compare` _                     = LT
  _                     `compare` (OrdValue (Number _)) = GT

  (OrdValue (String x)) `compare` (OrdValue (String y)) = x `compare` y
  (OrdValue (String _)) `compare` _                     = LT
  _                     `compare` (OrdValue (String _)) = GT

  (OrdValue (Array xs)) `compare` (OrdValue (Array ys)) = (OrdValue <$> xs) `compare` (OrdValue <$> ys)
  (OrdValue (Array _))  `compare` _                     = LT
  _                     `compare` (OrdValue (Array _))  = GT

  (OrdValue (Object x)) `compare` (OrdValue (Object y)) = H.toList (OrdValue <$> x) `compare` H.toList (OrdValue <$> y)
