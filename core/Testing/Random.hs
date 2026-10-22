-- | Pseudo-random generator (module 3, exercise 1): seed in, value and next seed out,
-- since @random@ is not in @base@.
module Testing.Random (Seed, next, mkSeed, nextInRange) where

import Data.Bits ((.&.), (.<<.), (.>>.), (.|.))
import Data.Word (Word64)

newtype Seed = Seed Word64 deriving (Eq, Show)

lowBitsMask :: Word64
lowBitsMask = 0xFFFFFFFF

mkSeed :: Word64 -> Seed
mkSeed w = Seed (w .&. lowBitsMask)

next :: Seed -> (Word64, Seed)
next (Seed w) = (w' .>>. 16, Seed w')
  where
    a = 1664525
    c = 1013904223
    w' = (a * w + c) .&. lowBitsMask

-- | Since a 16 bits number has a maximum value of 65_535 this functions builds a 32 bits
-- number using 16 bits composition
next32 :: Seed -> (Word64, Seed)
next32 s = (w, s2)
  where
    (w1, s1) = next s
    (w2, s2) = next s1
    w = (w2 .<<. 16) .|. w1

-- | A number between the two bounds, inclusive. Swapped bounds are read as the
-- same range rather than returning an Either, since a single error would halt
-- every generator chained after it. The value comes from 'next32', so a range
-- with more than 2^32 values does not reach all of them.
nextInRange :: Int -> Int -> Seed -> (Int, Seed)
nextInRange lo hi s = (fromIntegral v, s')
  where
    -- Integer, because hi - lo + 1 wraps to 0 in Int for [minBound, maxBound].
    -- Converting back is safe: the mod keeps v within [lo', hi'], both Ints.
    lo' = toInteger $ lo `min` hi
    hi' = toInteger $ lo `max` hi
    (w, s') = next32 s
    v = lo' + fromIntegral w `mod` (hi' - lo' + 1)
