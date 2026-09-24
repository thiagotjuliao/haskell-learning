module Main (main) where

import Testing.Runner

tests :: [TestGroup]
tests =
  let
    g1 = group "arith" [assertEqual "sum" 4 (2 + 2), assertEqual "mul" 1 (1 * 1)]
    g2 = group "strings" [assertEqual "concat" "abc" ("ab" ++ "c")]
   in
    [g1, g2]

main :: IO ()
main = do
  putStrLn "[core] Running tests...\n"
  runTests tests
