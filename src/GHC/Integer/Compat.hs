{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Trustworthy #-}

module GHC.Integer.Compat
    ( divInteger
    , quotRemInteger
    , quotInteger
    , IntegerClass(..)
    , classifyInteger
    ) where

import GHC.Integer        (quotRemInteger, quotInteger)

#if MIN_VERSION_base(4,15,0)
import GHC.Integer (divInteger)
import GHC.Num     (Integer(IS, IP, IN))
import GHC.Exts    (Int(I#))
#else

#ifdef MIN_VERSION_integer_simple

#if MIN_VERSION_integer_simple(0,1,1)
import GHC.Integer (divInteger)
#else
divInteger :: Integer -> Integer -> Integer
divInteger = div
#endif

#else

import GHC.Integer.GMP.Internals (Integer(S#, Jp#, Jn#))
import GHC.Exts                  (Int(I#))

#if MIN_VERSION_integer_gmp(0,5,1)
import GHC.Integer (divInteger)
#else
divInteger :: Integer -> Integer -> Integer
divInteger = div
#endif

#endif
#endif

-- | Where an 'Integer' lies relative to the 'Int' range.
data IntegerClass
    = SmallInt !Int
      -- ^ The value fits in an 'Int'.
    | LargePositive
      -- ^ The value is above @maxBound :: Int@.
    | LargeNegative
      -- ^ The value is below @minBound :: Int@.

-- | Classify an 'Integer' relative to the 'Int' range.
--
-- With @ghc-bignum@ and @integer-gmp@ this is a match on the constructor, so
-- no comparisons are needed. The small constructor holds the value as an
-- 'Int', and the two big constructors encode the sign.
--
-- This depends on the invariant that the small constructor is used exactly
-- when the value fits in an 'Int', so a big constructor never holds a value
-- in the 'Int' range (even e.g. after subtracting two big numbers):
--
-- * @ghc-bignum@ ('IS', 'IP', 'IN'): see the 'Integer' type and
--   @integerCheck#@ in
--   <https://hackage.haskell.org/package/ghc-bignum/docs/GHC-Num-Integer.html>
--   (source: <https://hackage.haskell.org/package/ghc-bignum/docs/src/GHC.Num.Integer.html>)
--
-- * @integer-gmp@ 1.0 ('S#', 'Jp#', 'Jn#'): see the 'Integer' type in
--   <https://hackage.haskell.org/package/integer-gmp-1.0.3.0/docs/GHC-Integer-GMP-Internals.html>
--
-- @integer-simple@ has no such invariant, so it uses range checks.
classifyInteger :: Integer -> IntegerClass
#if MIN_VERSION_base(4,15,0)
classifyInteger (IS i) = SmallInt (I# i)
classifyInteger (IP _) = LargePositive
classifyInteger (IN _) = LargeNegative
#elif defined(MIN_VERSION_integer_simple)
classifyInteger i
    | i > toInteger (maxBound :: Int) = LargePositive
    | i < toInteger (minBound :: Int) = LargeNegative
    | otherwise                       = SmallInt (fromInteger i)
#else
classifyInteger (S# i)  = SmallInt (I# i)
classifyInteger (Jp# _) = LargePositive
classifyInteger (Jn# _) = LargeNegative
#endif
{-# INLINE classifyInteger #-}
