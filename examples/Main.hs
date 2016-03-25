
module Main where

import           Test.Tasty        (TestTree, defaultMain, testGroup)
import qualified Test.Tasty.HUnit  as HU

import qualified CustomSchema      as C
import qualified PrettyShowFailure as P
import qualified Standard          as S

main :: IO ()
main = defaultMain (testGroup "Check Examples" exampleTests)

exampleTests :: [TestTree]
exampleTests =
  [ HU.testCase "Standard Example" S.example
  , HU.testCase "PrettyShowFailure Example" P.example
  , HU.testCase "CustomSchema Example" C.example
  ]
