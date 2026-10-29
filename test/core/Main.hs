module Main (main) where

import qualified M01.ExprSpec as Expr
import qualified M01.ListPreludeFoldrSpec as ListPreludeFoldr
import qualified M01.ListPreludePointFreeSpec as ListPreludePointFree
import qualified M01.ListPreludeSpec as ListPrelude
import qualified M02.FractionSpec as Fraction
import qualified M02.InstancesSpec as Instances
import qualified M02.MatrixSpec as Matrix
import qualified M03.DerivingSpec as Deriving
import qualified M03.MonoidsSpec as Monoids
import qualified M03.StatsSpec as Stats
import qualified M04.FileSystemSpec as FileSystem
import qualified M04.FunctorsSpec as Functors
import qualified M04.LawsSpec as Laws
import qualified M04.TreeSpec as Tree
import qualified Testing.CheckSpec as Check
import qualified Testing.RandomSpec as Random
import Testing.Runner

tests :: [TestGroup]
tests =
  sanity
    ++ ListPrelude.tests
    ++ ListPreludeFoldr.tests
    ++ ListPreludePointFree.tests
    ++ Expr.tests
    ++ Instances.tests
    ++ Fraction.tests
    ++ Matrix.tests
    ++ Random.tests
    ++ Check.tests
    ++ Monoids.tests
    ++ Deriving.tests
    ++ Stats.tests
    ++ Functors.tests
    ++ Tree.tests
    ++ Laws.tests
    ++ FileSystem.tests

-- | Smoke tests for the runner itself.
sanity :: [TestGroup]
sanity =
  [ group "arith" [assertEqual "sum" 4 (2 + 2 :: Int), assertEqual "mul" 1 (1 * 1 :: Int)]
  , group "strings" [assertEqual "concat" "abc" ("ab" ++ "c")]
  ]

main :: IO ()
main = do
  putStrLn "[core] Running tests...\n"
  runTests tests
