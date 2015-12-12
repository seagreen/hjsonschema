
module Main where

import qualified CustomSchema      as C
import qualified PrettyShowFailure as P
import qualified Standard          as S

main :: IO ()
main = do
  putStrLn "# Standard Example"
  S.example
  putStrLn ""

  putStrLn "# PrettyShowFailure Example"
  P.example
  putStrLn ""

  putStrLn "# CustomSchema Example"
  C.example
