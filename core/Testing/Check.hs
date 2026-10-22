-- | Mini-QuickCheck (module 3, exercise 2): generators, an Arbitrary class and forAll.
module Testing.Check (
  Gen,
  run,
  range,
  oneOf,
  listOf,
  pair,
  constant,
  andThen,
  mapGen,
  Arbitrary (..),
  forAll,
) where

import Testing.Random (Seed, mkSeed, nextInRange)
import Testing.Runner (TestResult, failure, hasFailed, success)

newtype Gen a = Gen (Seed -> (a, Seed))

run :: Gen a -> Seed -> (a, Seed)
run (Gen f) s = f s

constant :: a -> Gen a
constant a = Gen (\s -> (a, s))

mapGen :: (a -> b) -> Gen a -> Gen b
mapGen f ga = ga `andThen` \a -> constant (f a)

andThen :: Gen a -> (a -> Gen b) -> Gen b
andThen ga f = Gen f'
  where
    f' s = (b, s'')
      where
        (a, s') = run ga s
        (b, s'') = run (f a) s'

range :: Int -> Int -> Gen Int
range lo hi = Gen (nextInRange lo hi)

at :: a -> Int -> [a] -> a
at a _ [] = a
at a n (h : t)
  | n == 0 = h
  | otherwise = at a (n - 1) t

oneOf :: a -> [a] -> Gen a
oneOf a as = range 0 (length as) `andThen` \i -> constant (at a i as)

listOf :: Gen a -> Gen [a]
listOf ga = len `andThen` \n -> f n
  where
    len = range 0 20
    f n =
      foldr
        (\g gs -> g `andThen` \a -> gs `andThen` \as -> constant (a : as))
        (constant [])
        (replicate n ga)

pair :: Gen a -> Gen b -> Gen (a, b)
pair ga gb = ga `andThen` \a -> (gb `andThen` \b -> constant (a, b))

-- Arbitraries
class Arbitrary a where
  arbitrary :: Gen a

instance Arbitrary Int where
  arbitrary = range (-1000) 1000

instance Arbitrary Integer where
  arbitrary = mapGen fromIntegral $ range (-1000) 1000

instance Arbitrary Bool where
  arbitrary = mapGen (== 0) $ range 0 1

instance (Arbitrary a) => Arbitrary [a] where
  arbitrary = listOf arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = pair arbitrary arbitrary

errorMsg :: (Show a) => String -> a -> Seed -> String
errorMsg name a s =
  "property "
    ++ name
    ++ " failed for value "
    ++ show a
    ++ " with "
    ++ show s

check :: (Show a) => String -> (a -> Bool) -> a -> Seed -> TestResult
check name p a s = if p a then success name else failure name (errorMsg name a s)

forAll :: (Show a) => String -> Gen a -> (a -> Bool) -> TestResult
forAll name g p =
  case res of
    [] -> success name
    (f : _) -> f
  where
    seeds = [mkSeed i | i <- [0 .. 99]]
    as = [fst $ run g s | s <- seeds]
    trs = uncurry (check name p) <$> (as `zip` seeds)
    res = dropWhile (not . hasFailed) trs
