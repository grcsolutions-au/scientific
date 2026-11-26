{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
    ( roundTo
    , i2d
    , maxExpt
    , magnitude
    , scaledInt
    ) where

#include "MachDeps.h"

import GHC.Base (Int(I#), Char(C#), chr#, ord#, (+#))

import GHC.Integer.Compat (IntegerClass(..))

import qualified Data.Primitive.Array as Primitive
import           Control.Monad.ST             (runST)

import           Data.Bits                    (unsafeShiftR)

roundTo :: Int -> [Int] -> (Int, [Int])
roundTo d is =
  case f d True is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
    _       -> error "roundTo: bad Value"
 where
  base = 10

  b2 = base `quot` 2

  f n _ []     = (0, replicate n 0)
  f 0 e (x:xs) | x == b2 && e && all (== 0) xs = (0, [])   -- Round to even when at exactly half the base
               | otherwise = (if x >= b2 then 1 else 0, [])
  f n _ (i:xs)
     | i' == base = (1,0:ds)
     | otherwise  = (0,i':ds)
      where
       (c,ds) = f (n-1) (even i) xs
       i'     = c + i

-- | Unsafe conversion for decimal digits.
{-# INLINE i2d #-}
i2d :: Int -> Char
i2d (I# i#) = C# (chr# (ord# '0'# +# i# ))

----------------------------------------------------------------------
-- Exponentiation with a cache for the most common numbers.
----------------------------------------------------------------------

-- | The same limit as in GHC.Float.
maxExpt :: Int
maxExpt = 324

expts10 :: Primitive.Array Integer
expts10 = runST $ do
    ma <- Primitive.newArray maxExpt uninitialised
    Primitive.writeArray ma 0  1
    Primitive.writeArray ma 1 10
    let go !ix
          | ix == maxExpt = Primitive.unsafeFreezeArray ma
          | otherwise = do
              Primitive.writeArray ma  ix        xx
              Primitive.writeArray ma (ix+1) (10*xx)
              go (ix+2)
          where
            xx = x * x
            x  = Primitive.indexArray expts10 half
            !half = ix `unsafeShiftR` 1
    go 2

uninitialised :: error
uninitialised = error "Data.Scientific: uninitialised element"

-- | @magnitude e == 10 ^ e@
magnitude :: Num a => Int -> a
magnitude e | e < 0       = error "magnitude: with negative argument"
            | e < maxExpt = cachedPow10 e
            | otherwise   = cachedPow10 hi * 10 ^ (e - hi)
    where
      cachedPow10 = fromInteger . Primitive.indexArray expts10

      hi = maxExpt - 1

----------------------------------------------------------------------
-- Fast scaling of 'Int' coefficients.
----------------------------------------------------------------------

#if WORD_SIZE_IN_BITS != 32 && WORD_SIZE_IN_BITS != 64
#error "Utils: scaledInt only supports platforms with a 32 or 64 bit word size (WORD_SIZE_IN_BITS)"
#endif

-- | One table entry for a positive exponent: check @c@ against the
-- coefficient bounds @lo@ and @hi@, then multiply by @m == 10^e@. The
-- multiply can't overflow because the range check runs first.
checkScaled :: Int -> Int -> Int -> Int -> Maybe IntegerClass
checkScaled lo hi m c
    | c > hi    = Just LargePositive
    | c < lo    = Just LargeNegative
    | otherwise = Just (SmallInt (c * m))
{-# INLINE checkScaled #-}

-- | One table entry for a negative exponent: divide @c@ by @m == 10^(-e)@
-- if that is exact. The quotient always fits in an 'Int'.
exactScaled :: Int -> Int -> Maybe IntegerClass
exactScaled m c = case c `quotRem` m of
    (q, 0) -> Just (SmallInt q)
    _      -> Nothing
{-# INLINE exactScaled #-}

-- | Classify @c * 10^e@ relative to the 'Int' range for an 'Int'
-- coefficient, without 'Integer' arithmetic. 'Nothing' means @c * 10^e@
-- isn't an integer.
--
-- For a negative exponent the result is never 'LargePositive' or
-- 'LargeNegative', since dividing doesn't make @c@ larger. For a
-- non-negative exponent it is never 'Nothing'.
--
-- This is a hard-coded table, written as a @case@ on @e@. A positive
-- exponent's entry holds the coefficient bounds (@minBound `quot` 10^e@ and
-- @maxBound `quot` 10^e@ of 'Int') and @10^e@. A negative exponent's entry
-- holds @10^(-e)@. Past either end of the table only a zero coefficient
-- gives an integer in the 'Int' range. Matching on @e@ itself, rather than
-- @negate e@, means @e == minBound@ needs no special care.
scaledInt :: Int -> Int -> Maybe IntegerClass
scaledInt c e = case e of
#if WORD_SIZE_IN_BITS == 64
    -18 -> exactScaled 1000000000000000000 c
    -17 -> exactScaled 100000000000000000 c
    -16 -> exactScaled 10000000000000000 c
    -15 -> exactScaled 1000000000000000 c
    -14 -> exactScaled 100000000000000 c
    -13 -> exactScaled 10000000000000 c
    -12 -> exactScaled 1000000000000 c
    -11 -> exactScaled 100000000000 c
    -10 -> exactScaled 10000000000 c
#endif
    -9 -> exactScaled 1000000000 c
    -8 -> exactScaled 100000000 c
    -7 -> exactScaled 10000000 c
    -6 -> exactScaled 1000000 c
    -5 -> exactScaled 100000 c
    -4 -> exactScaled 10000 c
    -3 -> exactScaled 1000 c
    -2 -> exactScaled 100 c
    -1 -> exactScaled 10 c
    -- Every coefficient fits.
    0 -> Just (SmallInt c)
#if WORD_SIZE_IN_BITS == 64
    1 -> checkScaled (-922337203685477580) 922337203685477580 10 c
    2 -> checkScaled (-92233720368547758) 92233720368547758 100 c
    3 -> checkScaled (-9223372036854775) 9223372036854775 1000 c
    4 -> checkScaled (-922337203685477) 922337203685477 10000 c
    5 -> checkScaled (-92233720368547) 92233720368547 100000 c
    6 -> checkScaled (-9223372036854) 9223372036854 1000000 c
    7 -> checkScaled (-922337203685) 922337203685 10000000 c
    8 -> checkScaled (-92233720368) 92233720368 100000000 c
    9 -> checkScaled (-9223372036) 9223372036 1000000000 c
    10 -> checkScaled (-922337203) 922337203 10000000000 c
    11 -> checkScaled (-92233720) 92233720 100000000000 c
    12 -> checkScaled (-9223372) 9223372 1000000000000 c
    13 -> checkScaled (-922337) 922337 10000000000000 c
    14 -> checkScaled (-92233) 92233 100000000000000 c
    15 -> checkScaled (-9223) 9223 1000000000000000 c
    16 -> checkScaled (-922) 922 10000000000000000 c
    17 -> checkScaled (-92) 92 100000000000000000 c
    18 -> checkScaled (-9) 9 1000000000000000000 c
#else
    1 -> checkScaled (-214748364) 214748364 10 c
    2 -> checkScaled (-21474836) 21474836 100 c
    3 -> checkScaled (-2147483) 2147483 1000 c
    4 -> checkScaled (-214748) 214748 10000 c
    5 -> checkScaled (-21474) 21474 100000 c
    6 -> checkScaled (-2147) 2147 1000000 c
    7 -> checkScaled (-214) 214 10000000 c
    8 -> checkScaled (-21) 21 100000000 c
    9 -> checkScaled (-2) 2 1000000000 c
#endif
    -- Past either end of the table.
    _ | c == 0    -> Just (SmallInt 0)
      -- @|c| < 10^(-e)@, so @c * 10^e@ has a fractional part.
      | e < 0     -> Nothing
      | c > 0     -> Just LargePositive
      | otherwise -> Just LargeNegative
{-# INLINE scaledInt #-}
