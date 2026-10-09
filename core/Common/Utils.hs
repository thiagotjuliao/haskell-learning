module Common.Utils (thenCompare) where

thenCompare :: Ordering -> Ordering -> Ordering
thenCompare EQ c = c
thenCompare c _ = c
