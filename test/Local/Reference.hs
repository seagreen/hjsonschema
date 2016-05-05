
module Local.Reference where

import           Test.Tasty               (TestTree)
import qualified Test.Tasty.HUnit         as HU

import           Data.Validator.Reference

referenceTests :: [TestTree]
referenceTests =
  [ HU.testCase "updateResolutionScope test cases" updateResolutionScopeTests
  , HU.testCase "resolveReference test cases" resolveReferenceTests
  ]

updateResolutionScopeTests :: IO ()
updateResolutionScopeTests = do
  HU.assertEqual
    "case 1 result"
    Nothing
    (updateResolutionScope Nothing Nothing)
  HU.assertEqual
    "case 2 result"
    Nothing
    (updateResolutionScope Nothing (Just "#"))
  HU.assertEqual
    "case 3 result"
    (Just "foo")
    (updateResolutionScope Nothing (Just "foo"))
  HU.assertEqual
    "case 4 result"
    (Just "/./bar") -- TODO: Normalize after updateResolutionScope.
    (updateResolutionScope (Just "/foo") (Just "./bar"))

resolveReferenceTests :: IO ()
resolveReferenceTests = do
  HU.assertEqual
    "case 1 result"
    (Just "/bar", Nothing)
    (resolveReference (Just "/foo") "bar")
  HU.assertEqual
    "case 2 result"
    (Just "/baz", Nothing)
    (resolveReference (Just "/foo/bar") "/baz")
  HU.assertEqual
    "case 3 result"
    (Nothing, Just "/bar")
    (resolveReference Nothing "#/bar")
  HU.assertEqual
    "case 4 result"
    (Just "/foo", Just "/bar")
    (resolveReference (Just "/foo") "#/bar")
