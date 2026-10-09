-- | Tests for "M02.Fraction", with 'Rational' from "Data.Ratio" as the oracle:
-- every operation on a 'Fraction' must give the value 'Rational' gives on the
-- same inputs.
--
-- The two are compared through 'show', with @%@ read as @/@. 'Rational' prints
-- its reduced form with a positive denominator, so a matching 'show' also
-- checks the invariant, without reaching for the 'Fraction' constructor.
-- 'show' itself is pinned down first, by hand.
--
-- Division by zero ('recip' 0, @1 / 0@) throws, and the runner cannot catch an
-- exception, so those cases are left out.
module M02.FractionSpec (tests) where

import Data.Ratio ((%))
import M02.Fraction
import Testing.Runner

tests :: [TestGroup]
tests = [construction, showTests, comparison, arithmetic, laws]

-- | Numerators and denominators covering signs, zero, and pairs that reduce.
raw :: [(Integer, Integer)]
raw = [(-6, 4), (-1, 3), (0, 5), (1, 1), (2, -4), (3, 7), (5, 2), (-4, -6)]

-- | Each sample as a 'Fraction' next to the same value as a 'Rational'.
samples :: [(Fraction, Rational)]
samples = [(f, n % d) | (n, d) <- raw, Just f <- [fraction n d]]

pairs :: [((Fraction, Rational), (Fraction, Rational))]
pairs = [(x, y) | x <- samples, y <- samples]

fractions :: [Fraction]
fractions = map fst samples

-- | The oracle's output in the fraction's notation.
asFraction :: Rational -> String
asFraction = map (\c -> if c == '%' then '/' else c) . show

-- | Applies a unary operation to both sides and compares the results.
agreesOn1 :: String -> (Fraction -> Fraction) -> (Rational -> Rational) -> TestResult
agreesOn1 name f r =
  assertEqual name [asFraction (r q) | (_, q) <- samples] [show (f x) | (x, _) <- samples]

-- | Applies a binary operation to every pair on both sides.
agreesOn2 ::
  String -> (Fraction -> Fraction -> Fraction) -> (Rational -> Rational -> Rational) -> TestResult
agreesOn2 name f r =
  assertEqual
    name
    [asFraction (r q1 q2) | ((_, q1), (_, q2)) <- pairs]
    [show (f x1 x2) | ((x1, _), (x2, _)) <- pairs]

construction :: TestGroup
construction =
  group
    "Fraction: construction"
    [ assertEqual "zero denominator" Nothing (fmap show (fraction 1 0))
    , assertEqual "no sample rejected" (length raw) (length samples)
    , assertEqual
        "reduced, positive denominator"
        [asFraction (n % d) | (n, d) <- raw]
        (map show fractions)
    , agreesOn1 "fromInteger" (const 7) (const 7)
    , assertEqual "fromRational" (map asFraction [0.75, -2.5]) (map show [0.75, -2.5 :: Fraction])
    ]

showTests :: TestGroup
showTests =
  group
    "Fraction: Show"
    [ assertEqual "positive" "3 / 4" (show (0.75 :: Fraction))
    , assertEqual "negative numerator" "(-1) / 2" (show (-0.5 :: Fraction))
    , assertEqual "integer" "3 / 1" (show (3 :: Fraction))
    , assertEqual "inside a constructor" "Just ((-1) / 2)" (show (Just (-0.5 :: Fraction)))
    , assertEqual "precedence 7, left of *" "1 / 2" (showsPrec 7 (0.5 :: Fraction) "")
    , assertEqual "precedence 8, operand of ^" "(1 / 2)" (showsPrec 8 (0.5 :: Fraction) "")
    , assertEqual "list" "[1 / 2,(-1) / 3]" (show [0.5, negate (1 / 3) :: Fraction])
    ]

comparison :: TestGroup
comparison =
  group
    "Fraction: Eq, Ord"
    [ assertEqual
        "=="
        [q1 == q2 | ((_, q1), (_, q2)) <- pairs]
        [x1 == x2 | ((x1, _), (x2, _)) <- pairs]
    , assertEqual
        "compare"
        [compare q1 q2 | ((_, q1), (_, q2)) <- pairs]
        [compare x1 x2 | ((x1, _), (x2, _)) <- pairs]
    , assertEqual "equal after reduction" (fraction 1 2) (fraction (-2) (-4))
    ]

arithmetic :: TestGroup
arithmetic =
  group
    "Fraction: Num, Fractional"
    [ agreesOn2 "+" (+) (+)
    , agreesOn2 "-" (-) (-)
    , agreesOn2 "*" (*) (*)
    , agreesOn1 "negate" negate negate
    , agreesOn1 "abs" abs abs
    , agreesOn1 "signum" signum signum
    , assertEqual
        "/, nonzero divisors"
        [asFraction (q1 / q2) | ((_, q1), (_, q2)) <- pairs, q2 /= 0]
        [show (x1 / x2) | ((x1, _), (x2, q2)) <- pairs, q2 /= 0]
    , assertEqual
        "recip, nonzero"
        [asFraction (recip q) | (_, q) <- samples, q /= 0]
        [show (recip x) | (x, q) <- samples, q /= 0]
    , assertEqual "recip of a negative, by hand" (fraction (-2) 1) (Just (recip (-0.5)))
    ]

-- | The ring laws and the 'signum' law, checked with the instances alone. The
-- oracle already implies them; these name the property that broke when one
-- fails.
laws :: TestGroup
laws =
  group
    "Fraction: laws"
    [ assertEqual "+ associative" [] [t | t@(x, y, z) <- triples, (x + y) + z /= x + (y + z)]
    , assertEqual "* associative" [] [t | t@(x, y, z) <- triples, (x * y) * z /= x * (y * z)]
    , assertEqual
        "* distributes over +"
        []
        [t | t@(x, y, z) <- triples, x * (y + z) /= x * y + x * z]
    , assertEqual "+ commutative" [] [(x, y) | x <- fractions, y <- fractions, x + y /= y + x]
    , assertEqual "* commutative" [] [(x, y) | x <- fractions, y <- fractions, x * y /= y * x]
    , assertEqual "identities" [] [x | x <- fractions, x + 0 /= x || x * 1 /= x]
    , assertEqual "additive inverse" [] [x | x <- fractions, x + negate x /= 0]
    , assertEqual "abs x * signum x == x" [] [x | x <- fractions, abs x * signum x /= x]
    , assertEqual "x * recip x == 1" [] [x | x <- fractions, x /= 0, x * recip x /= 1]
    ]
  where
    triples :: [(Fraction, Fraction, Fraction)]
    triples = [(x, y, z) | x <- fractions, y <- fractions, z <- fractions]
