-- | Tests for "Testing.Random".
module Testing.RandomSpec (tests) where

import Testing.Random (Seed, mkSeed, next, nextInRange)
import Testing.Runner

tests :: [TestGroup]
tests = [sameSeed, differentSeeds, ranges]

generate :: Int -> (Seed -> (a, Seed)) -> Seed -> ([a], Seed)
generate n g s
  | n <= 0 = ([], s)
  | otherwise = (w : r, s'')
  where
    (w, s') = g s
    (r, s'') = generate (n - 1) g s'

-- | 'generate' is pure, so comparing it with itself could never fail: the
-- sequence is pinned to fixed values instead. The first one was worked out by
-- hand, (1664525 * 42 + 1013904223) mod 2^32 = 1083814273, shifted right by 16
-- = 16537; the rest come from GHCi and guard against the algorithm changing.
sameSeed :: TestGroup
sameSeed =
  group
    "Random: the same seed gives the same sequence"
    [ assertEqual
        "five values from seed 42"
        ([16537, 5775, 37832, 14585, 24619], mkSeed 1613448261)
        (generate 5 next (mkSeed 42))
    , assertEqual
        "the state is the unshifted LCG step"
        (16537, mkSeed 1083814273)
        (next (mkSeed 42))
    ]

differentSeeds :: TestGroup
differentSeeds =
  group
    "Random: different seeds give different sequences"
    [ assertEqual "seeds 1 and 2" False (values 1 == values 2)
    , assertEqual "seeds 42 and 43" False (values 42 == values 43)
    , -- The state has 32 bits, so seeds that differ only above them collapse.
      assertEqual "mkSeed keeps the low 32 bits" (mkSeed 5) (mkSeed (2 ^ (32 :: Int) + 5))
    ]
  where
    values w = fst (generate 10 next (mkSeed w))

ranges :: TestGroup
ranges =
  group
    "Random: values stay inside the range and reach both ends"
    [ assertEqual "a die: [1, 6]" (1, 6) (bounds 1000 1 6)
    , assertEqual "negative bounds: [-3, 3]" (-3, 3) (bounds 1000 (-3) 3)
    , assertEqual "a single value: [7, 7]" (7, 7) (bounds 10 7 7)
    , -- next gives 16 bits, so values above 65535 need next32. Hitting both
      -- exact ends of 100001 values would take far more draws than a test can
      -- afford: from seed 42, 200000 draws never produced 0.
      assertEqual
        "wider than 16 bits: [0, 100000]"
        True
        (wideLo >= 0 && wideHi <= 100000 && wideHi > 65535)
    , assertEqual
        "swapped bounds mean the same range"
        (fst (generate 20 (nextInRange 1 6) (mkSeed 42)))
        (fst (generate 20 (nextInRange 6 1) (mkSeed 42)))
    , -- maxBound - minBound + 1 wraps to 0 in Int, so this divided by zero
      -- before the arithmetic moved to Integer.
      assertEqual
        "the whole Int range does not divide by zero"
        (-9223372036476288871)
        (fst (nextInRange minBound maxBound (mkSeed 42)))
    ]
  where
    (wideLo, wideHi) = bounds 10000 0 100000
    -- minimum and maximum are partial, but every call above asks for n > 0.
    bounds :: Int -> Int -> Int -> (Int, Int)
    bounds n lo hi = (minimum xs, maximum xs)
      where
        xs = fst (generate n (nextInRange lo hi) (mkSeed 42))
