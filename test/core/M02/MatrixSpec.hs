-- | Tests for "M02.Matrix": the 'Num' instance by hand and through the laws
-- a matrix ring keeps, and 'fib' against a plain Fibonacci.
--
-- 'abs' and 'signum' throw by design, and the runner cannot catch an
-- exception, so they are left out.
module M02.MatrixSpec (tests) where

import M02.Matrix
import Numeric.Natural (Natural)
import Testing.Runner

tests :: [TestGroup]
tests = [numTests, laws, fibTests]

-- | Small entries with every sign, the zero and identity matrices, and the
-- Fibonacci matrix.
samples :: [Matrix Integer]
samples =
  [ Matrix 1 2 3 4
  , Matrix 0 1 1 0
  , Matrix (-2) 0 5 (-1)
  , Matrix 2 (-3) 0 7
  , Matrix 1 1 1 0
  , 0
  , 1
  ]

numTests :: TestGroup
numTests =
  group
    "Matrix: Num, Show"
    [ assertEqual "+" (Matrix 6 8 10 12) (Matrix 1 2 3 4 + Matrix 5 6 7 (8 :: Integer))
    , assertEqual "-" (Matrix (-4) (-4) (-4) (-4)) (Matrix 1 2 3 4 - Matrix 5 6 7 (8 :: Integer))
    , assertEqual "*" (Matrix 19 22 43 50) (Matrix 1 2 3 4 * Matrix 5 6 7 (8 :: Integer))
    , assertEqual "negate" (Matrix (-1) 2 (-3) (-4)) (negate (Matrix 1 (-2) 3 (4 :: Integer)))
    , assertEqual "fromInteger is a scalar matrix" (Matrix 3 0 0 3) (3 :: Matrix Integer)
    , assertEqual "scalar times a matrix" (Matrix 3 6 9 12) (3 * Matrix 1 2 3 (4 :: Integer))
    , assertEqual "power 0 is the identity" (Matrix 1 0 0 1) (Matrix 1 2 3 (4 :: Integer) ^ (0 :: Int))
    , assertEqual "power 10" (Matrix 89 55 55 34) (Matrix 1 1 1 (0 :: Integer) ^ (10 :: Int))
    , assertEqual "* does not commute" False (a * b == b * a)
    , assertEqual "show" "Matrix 1 (-2) 3 4" (show (Matrix 1 (-2) 3 (4 :: Int)))
    , assertEqual "show inside a constructor" "Just (Matrix 1 0 0 1)" (show (Just (1 :: Matrix Int)))
    ]
  where
    a, b :: Matrix Integer
    a = Matrix 1 2 3 4
    b = Matrix 0 1 1 0

-- | The ring laws that hold for matrices; '*' is not commutative, so that one
-- is checked above as a counterexample instead. Each case lists the values
-- that break the law, empty when it holds.
laws :: TestGroup
laws =
  group
    "Matrix: laws"
    [ assertEqual "+ associative" [] [t | t@(x, y, z) <- triples, (x + y) + z /= x + (y + z)]
    , assertEqual "+ commutative" [] [(x, y) | x <- samples, y <- samples, x + y /= y + x]
    , assertEqual "* associative" [] [t | t@(x, y, z) <- triples, (x * y) * z /= x * (y * z)]
    , assertEqual
        "* distributes over +, left"
        []
        [t | t@(x, y, z) <- triples, x * (y + z) /= x * y + x * z]
    , assertEqual
        "* distributes over +, right"
        []
        [t | t@(x, y, z) <- triples, (y + z) * x /= y * x + z * x]
    , assertEqual "identities" [] [x | x <- samples, x + 0 /= x || x * 1 /= x || 1 * x /= x]
    , assertEqual "additive inverse" [] [x | x <- samples, x + negate x /= 0 || x - x /= 0]
    , assertEqual
        "fromInteger respects + and *"
        []
        [ (m, n)
        | m <- [-2 .. 3]
        , n <- [-2 .. 3]
        , fromInteger (m + n) /= (fromInteger m + fromInteger n :: Matrix Integer)
            || fromInteger (m * n) /= (fromInteger m * fromInteger n :: Matrix Integer)
        ]
    ]
  where
    triples :: [(Matrix Integer, Matrix Integer, Matrix Integer)]
    triples = [(x, y, z) | x <- samples, y <- samples, z <- samples]

-- | Linear-time Fibonacci, the reference 'fib' is checked against.
slowFib :: Natural -> Integer
slowFib n = go n 0 1
  where
    go :: Natural -> Integer -> Integer -> Integer
    go 0 a _ = a
    go k a b = go (k - 1) b (a + b)

fibTests :: TestGroup
fibTests =
  group
    "fib"
    [ assertEqual "fib 0" 0 (fib 0)
    , assertEqual "fib 1" 1 (fib 1)
    , assertEqual "fib 2" 1 (fib 2)
    , assertEqual "first 30, against a plain Fibonacci" (map slowFib [0 .. 30]) (map fib [0 .. 30])
    , assertEqual "fib 90, from the notes" 2880067194370816120 (fib 90)
    , assertEqual "fib 200 against a plain Fibonacci" (slowFib 200) (fib 200)
    ]
