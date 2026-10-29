-- | Tests for "M03.Stats": the monoid laws on 'Stats' built from generated
-- lists, every statistic against the same one computed on its own, and a list
-- folded in two parts against the list folded whole.
module M03.StatsSpec (tests) where

import M03.Monoids (Max (..), Min (..), Sum (..))
import M03.Stats
import Testing.Check (Arbitrary (..), Gen, andThen, forAll, mapGen, pair, range)
import Testing.Runner

tests :: [TestGroup]
tests = [laws, agreement, halves, examples]

-- | A 'Stats' from a list of up to 20 elements, built without 'mempty' unless
-- the list is empty. Going through 'stats' would start every value from
-- 'mempty', so a broken 'mempty' would hide inside every generated value and
-- the laws would hold anyway (a minimum taken with 'min' on 'Maybe', where
-- 'Nothing' wins, passed them that way).
statsOf :: (Arbitrary a, Num a, Ord a) => Gen (Stats a)
statsOf = mapGen build arbitrary
  where
    build xs = case xs of
      [] -> mempty
      y : ys -> foldr ((<>) . fromValue) (fromValue y) ys

laws :: TestGroup
laws =
  group "Stats: monoid laws" $
    monoidLaws "Stats Int" (statsOf :: Gen (Stats Int))
      ++ monoidLaws "Stats Integer" (statsOf :: Gen (Stats Integer))

-- | @(x <> y) <> z == x <> (y <> z)@ and @mempty@ on both sides, on generated
-- values.
monoidLaws :: (Eq a, Show a, Monoid a) => String -> Gen a -> [TestResult]
monoidLaws name g =
  [ forAll
      (name ++ ": associative")
      (pair (pair g g) g)
      (\((x, y), z) -> (x <> y) <> z == x <> (y <> z))
  , forAll (name ++ ": identity") g (\x -> mempty <> x == x && x <> mempty == x)
  ]

-- | Each statistic of the single pass against a fold that computes only it.
agreement :: TestGroup
agreement =
  group
    "Stats: agrees with each statistic computed separately"
    [ check "count" (\xs -> getCount (stats xs) == length xs)
    , check "sum" (\xs -> getSum (stats xs) == Sum (sum xs))
    , check "minimum" (\xs -> getMin (stats xs) == fmap Min (extreme min xs))
    , check "maximum" (\xs -> getMax (stats xs) == fmap Max (extreme max xs))
    , check "mean" (\xs -> getMean (stats xs) == mean xs)
    ]
  where
    check :: String -> ([Int] -> Bool) -> TestResult
    check name = forAll name arbitrary
    -- Total minimum and maximum: Nothing for the empty list.
    extreme :: (Int -> Int -> Int) -> [Int] -> Maybe Int
    extreme f xs = case xs of
      [] -> Nothing
      y : ys -> Just (foldr f y ys)
    mean :: [Int] -> Maybe Double
    mean xs
      | null xs = Nothing
      | otherwise = Just (fromIntegral (sum xs) / fromIntegral (length xs))

-- | A list split at every point, the ends included, folded in two parts and
-- combined: the reason for a monoid is that the parts can be folded apart.
halves :: TestGroup
halves =
  group
    "Stats: two parts combined equal the whole"
    [ forAll
        "split anywhere"
        (arbitrary `andThen` \xs -> mapGen (\k -> (xs, k)) (range 0 (length xs)))
        ( \(xs, k) ->
            let (l, r) = splitAt k (xs :: [Int])
             in stats l <> stats r == stats xs
        )
    ]

examples :: TestGroup
examples =
  group
    "Stats: examples"
    [ assertEqual
        "the empty list has no minimum, maximum or mean"
        (0, Sum 0, Nothing, Nothing, Nothing)
        (summary ([] :: [Int]))
    , assertEqual
        "a list by hand"
        (5, Sum 14, Just (Min 1), Just (Max 5), Just 2.8)
        (summary [3, 1, 4, 1, 5 :: Int])
    , -- Only negative values: a minimum seeded with 0 or any default would show.
      assertEqual
        "only negative values"
        (2, Sum (-10), Just (Min (-7)), Just (Max (-3)), Just (-5))
        (summary [-3, -7 :: Int])
    , assertEqual "the mean keeps its fraction" (Just 1.5) (getMean (stats [1, 2 :: Int]))
    , -- Integer has no bounds, so this needs the Maybe, not a Bounded identity.
      assertEqual
        "Integer beyond Int's range"
        (Just (Min (-big)), Just (Max big))
        (let s = stats [big, -big] in (getMin s, getMax s))
    ]
  where
    summary :: (Real a) => [a] -> (Int, Sum a, Maybe (Min a), Maybe (Max a), Maybe Double)
    summary xs = let s = stats xs in (getCount s, getSum s, getMin s, getMax s, getMean s)
    big :: Integer
    big = 2 ^ (100 :: Int)
