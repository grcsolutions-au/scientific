{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

#include "MachDeps.h"

import           Control.Monad
import           Data.Int
import           Data.Word
import           Data.Scientific                    as Scientific
import           Test.Tasty
import           Test.Tasty.HUnit                          (testCase, (@?=), (@=?), Assertion, assertBool)
import           Test.QuickCheck                       (Property, (===), (.&&.))
import qualified Test.QuickCheck                    as QC
import           Test.Tasty.QuickCheck                 (testProperty)
import qualified Data.Binary                        as Binary (encode, decode)
import qualified Data.Text.Lazy                     as TL  (unpack)
import qualified Data.Text.Lazy.Builder             as TLB (toLazyText)
import qualified Data.Text.Lazy.Builder.Scientific  as T
import           Numeric ( floatToDigits )

import qualified Data.ByteString.Lazy.Char8         as BLC8
import qualified Data.ByteString.Builder.Scientific as B
import qualified Data.ByteString.Builder            as B
import           Text.ParserCombinators.ReadP (readP_to_S)

main :: IO ()
main = testMain $ testGroup "scientific"
  [ testGroup "DoS protection"
    [ testGroup "Eq"
      [ testCase "1e1000000" $ assertBool "" $ (read "1e1000000" :: Scientific) == (read "1e1000000" :: Scientific)
      , testCase "1e1000000 ineq" $ assertBool "" $ (read "1e1000000" :: Scientific) /= (read "1e1000002" :: Scientific)

      -- this also indirectly checks that 'read' is fast enough.
      , testCase "10...0" $ assertBool "" $
          (read "1e1000000" :: Scientific) ==
          (read ('1' : replicate 1000000 '0'))
      ]
    , testGroup "Ord"
      [ testCase "compare 1234e1000000 123e1000001" $
          compare (read "1234e1000000" :: Scientific) (read "123e1000001" :: Scientific) @?= GT

      , testCase "10...0" $
          compare (read "1e1000001" :: Scientific)
                  (read ('1' : replicate 1000000 '0' ++ "0"))
              @?= EQ
      , testCase "1...1" $
          compare (read "1e1000001" :: Scientific)
                  (read ('1' : replicate 1000000 '0' ++ "1"))
              @?= LT
      ]

    , testGroup "isInteger"
        [ testCase "1e1000000" $ True @=? isInteger (read "1e1000000" :: Scientific)
        , testCase "10...0e-1" $ True @=? isInteger (read $ '1' : replicate 1000000 '0' ++ "e-1" :: Scientific)
        , testCase "10...0e-10...0" $ True @=? isInteger (read $ '1' : replicate 1000000 '0' ++ "e-1000000" :: Scientific)
        , testCase "10...0e-20...0" $ False @=? isInteger (read $ '1' : replicate 1000000 '0' ++ "e-2000000" :: Scientific)
        ]

      -- negate minBound == minBound, which used to make isInteger_ call
      -- magnitude with a negative argument (an error) in 0.3.10.0 and before.
    , testGroup "exponent minBound"
        [ testCase "isInteger 1e(minBound)"      $ False @=? isInteger (scientific 1 minBound)
        , testCase "isInteger 123e(minBound)"    $ False @=? isInteger (scientific 123 minBound)
        , testCase "isInteger -5e(minBound)"     $ False @=? isInteger (scientific (-5) minBound)
        , testCase "isInteger 0e(minBound)"      $ True  @=? isInteger (scientific 0 minBound)
        , testCase "isInteger 1e(minBound + 1)"  $ False @=? isInteger (scientific 1 (minBound + 1))
        , testCase "isFloating 1e(minBound)"     $ True  @=? isFloating (scientific 1 minBound)
        , testCase "toBoundedInteger 1e(minBound)" $
            Nothing @=? toBoundedInteger @Int (scientific 1 minBound)
        , testCase "toBoundedInteger 0e(minBound)" $
            Just 0 @=? toBoundedInteger @Int (scientific 0 minBound)
        , testCase "toBoundedIntegerOrErrorReason 1e(minBound)" $
            Left NotAnInteger @=? toBoundedIntegerOrErrorReason @Int8 (scientific 1 minBound)
        , testCase "toBoundedIntegerOrErrorReason -5e(minBound)" $
            Left NotAnInteger @=? toBoundedIntegerOrErrorReason @Word8 (scientific (-5) minBound)
        , testCase "floatingOrInteger 1e(minBound)" $
            Left 0 @=? floatingOrInteger @Double @Integer (scientific 1 minBound)
        ]

    , testGroup "toBoundedInteger"
        [ testCase "1e1000000" $ Nothing @=? toBoundedInteger @Int (read "1e1000000")
        , testCase "10...0e-1" $ Nothing @=? toBoundedInteger @Int (read $ '1' : replicate 1000000 '0' ++ "e-1")
        ]

    , testGroup "floatingOrInteger"
        [ testCase "1e1000000" $ Right (10 ^ (1000000 :: Int) :: Integer) @=? floatingOrInteger @Double @Integer (read "1e1000000")
        , testCase "10...0e-1" $ Right (10 ^ ( 999999 :: Int) :: Integer) @=? floatingOrInteger @Double @Integer (read $ '1' : replicate 1000000 '0' ++ "e-1")
        ]

    , testGroup "normalize"
        [ testCase "1e1000000" $ True @=? isInteger (normalize (read "1e1000000" :: Scientific))
        , testCase "10...0e-1" $ True @=? isInteger (normalize (read $ '1' : replicate 1000000 '0' ++ "e-1" :: Scientific))
        , testCase "10...0e-10...0" $ True @=? isInteger (normalize (read $ '1' : replicate 1000000 '0' ++ "e-1000000" :: Scientific))
        , testCase "10...0e-20...0" $ False @=? isInteger (normalize (read $ '1' : replicate 1000000 '0' ++ "e-2000000" :: Scientific))
        ]

    , testGroup "toDecimalDigits"
        [ testCase "9...9" $ do
            let (ds, n) = toDecimalDigits (read $ replicate 1000000 '9')
            (1000000,1000000) @=? (length ds, n)
        ]

    , testGroup "RealFrac"
      [ testGroup "floor"
        [ testCase "1e1000000"   $ (floor (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (floor (read "-1e-1000000" :: Scientific) :: Int) @?= (-1)
        , testCase "1e-1000000"  $ (floor (read "1e-1000000"  :: Scientific) :: Int) @?= 0
        ]
      , testGroup "ceiling"
        [ testCase "1e1000000"   $ (ceiling (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (ceiling (read "-1e-1000000" :: Scientific) :: Int) @?= 0
        , testCase "1e-1000000"  $ (ceiling (read "1e-1000000"  :: Scientific) :: Int) @?= 1
        ]
      , testGroup "round"
        [ testCase "1e1000000"   $ (round (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (round (read "-1e-1000000" :: Scientific) :: Int) @?= 0
        , testCase "1e-1000000"  $ (round (read "1e-1000000"  :: Scientific) :: Int) @?= 0
        ]
      , testGroup "truncate"
        [ testCase "1e1000000"   $ (truncate (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (truncate (read "-1e-1000000" :: Scientific) :: Int) @?= 0
        , testCase "1e-1000000"  $ (truncate (read "1e-1000000"  :: Scientific) :: Int) @?= 0
        ]
      , testGroup "properFracton"
        [ testCase "1e1000000"   $ properFraction (read "1e1000000" :: Scientific) @?= (0 :: Int, 0)
        , testCase "-1e-1000000" $ let s = read "-1e-1000000" :: Scientific
                                   in properFraction s @?= (0 :: Int, s)
        , testCase "1e-1000000"  $ let s = read "1e-1000000" :: Scientific
                                   in properFraction s @?= (0 :: Int, s)
        ]
      ]
    , testGroup "toRealFloat"
      [ testCase "1e1000000"  $ assertBool "Should be infinity!" $ isInfinite $
                                  (toRealFloat (read "1e1000000" :: Scientific) :: Double)
      , testCase "1e-1000000" $ (toRealFloat (read "1e-1000000" :: Scientific) :: Double) @?= 0
      ]
    ]

  , testProperty "normalization"
       (QC.forAll normalizedScientificGen    $ \s ->
            s /= 0 QC.==> abs (Scientific.coefficient s) `mod` 10 /= 0)

  , testGroup "Binary"
    [ testProperty "decode . encode == id" $ \s ->
        Binary.decode (Binary.encode s) === theSci s
    ]

  , testGroup "Parsing"
    [ testCase "reads \"\""        $ testReads ""        []
    , testCase "reads \"1.\""      $ testReads "1."      [(1.0, ".")]
    , testCase "reads \"1.2e\""    $ testReads "1.2e"    [(1.2, "e")]
    , testCase "reads \"(1.3 )\""  $ testReads "(1.3 )"  [(1.3, "")]
    , testCase "reads \"((1.3))\"" $ testReads "((1.3))" [(1.3, "")]
    , testCase "reads \" 1.3\""    $ testReads " 1.3"    [(1.3, "")]
    , testCase "read \" ( ((  -1.0e+3 ) ))\"" $ testRead " ( ((  -1.0e+3 ) ))" (-1000.0)
    , testCase "scientificP \"3\""       $ testScientificP "3"       [(3.0, "")]
    , testCase "scientificP \"3.0e2\""   $ testScientificP "3.0e2"   [(3.0, "e2"), (300.0, "")]
    , testCase "scientificP \"+3.0e+2\"" $ testScientificP "+3.0e+2" [(3.0, "e+2"), (300.0, "")]
    , testCase "scientificP \"-3.0e-2\"" $ testScientificP "-3.0e-2" [(-3.0, "e-2"), (-3.0e-2, "")]
    ]

  , testGroup "Formatting"
    [ testProperty "read . show == id" $ \s -> read (show s) === theSci s
    , testCase "show (Just 1)"    $ testShow (Just 1)    "Just 1.0"
    , testCase "show (Just 0)"    $ testShow (Just 0)    "Just 0.0"
    , testCase "show (Just (-1))" $ testShow (Just (-1)) "Just (-1.0)"

    , testGroup "toDecimalDigits"
      [ testProperty "laws"
          (QC.forAll nonNegativeScientificGen    toDecimalDigits_laws)

      , testProperty "== Numeric.floatToDigits"
          (toDecimalDigits_eq_floatToDigits . QC.getNonNegative)
      ]

    , testGroup "Builder"
      [ testProperty "Text" $ \s ->
          formatScientific Scientific.Generic Nothing s ==
          TL.unpack (TLB.toLazyText $
                       T.formatScientificBuilder Scientific.Generic Nothing s)

      , testProperty "ByteString" $ \s ->
          formatScientific Scientific.Generic Nothing s ==
          BLC8.unpack (B.toLazyByteString $
                        B.formatScientificBuilder Scientific.Generic Nothing s)
      ]

    , testProperty "formatScientific_fromFloatDigits" $ \(d::Double) ->
        formatScientific Scientific.Generic Nothing (Scientific.fromFloatDigits d) ==
        show d

    -- , testProperty "formatScientific_realToFrac" $ \(d::Double) ->
    --     formatScientific B.Generic Nothing (realToFrac d :: Scientific) ==
    --     show d
    ]

  , testGroup "Eq"
    [ testProperty "==" $ \(s1 :: Scientific) (s2 :: Scientific) ->
        (s1 == s2) == (toRational s1 == toRational s2)
    , testProperty "s == s" $ \(s :: Scientific) -> s == s

    , testProperty "extreme" $
        -- This was true in scientific-0.3.8.0
        -- due 'normalize' inherent limitation causing overflow
        --
        -- in scientific-0.3.9.0 this crashed.
        not (scientific 10 maxBound == scientific 1 minBound)

    ]

  , testGroup "Ord"
    [ testProperty "compare" $ \(s1 :: Scientific) (s2 :: Scientific) ->
        compare s1 s2 == compare (toRational s1) (toRational s2)

    , testProperty "extreme" $
        -- This was true in scientific-0.3.8.0
        -- due 'normalize' inherent limitation causing overflow
        --
        -- in scientific-0.3.9.0 this crashed.
        compare (scientific 10 maxBound) (scientific 1 minBound) === GT
    ]

  , testGroup "Num"
    [ testGroup "Equal to Rational"
      [ testProperty "fromInteger" $ \i -> fromInteger i === theSci (fromRational (fromInteger i))
      , testProperty "+"           $ bin (+)
      , testProperty "-"           $ bin (-)
      , testProperty "*"           $ bin (*)
      , testProperty "abs"         $ unary abs
      , testProperty "negate"      $ unary negate
      , testProperty "signum"      $ unary signum
      ]

    , testProperty "0 identity of +" $ \a -> a + 0 === theSci a
    , testProperty "1 identity of *" $ \a -> 1 * a === theSci a
    , testProperty "0 identity of *" $ \a -> 0 * a === theSci 0

    , testProperty "associativity of +"         $ \a b c -> a + (b + c) === (a + b) + theSci c
    , testProperty "commutativity of +"         $ \a b   -> a + b       === b + theSci a
    , testProperty "distributivity of * over +" $ \a b c -> a * (b + c) === a * b + a * theSci c

    , testProperty "subtracting the addition" $ \x y -> x + y - y === theSci x

    , testProperty "+ and negate" $ \x -> theSci x + negate x === 0
    , testProperty "- and negate" $ \x -> theSci x - negate x === x + x

    , testProperty "abs . negate == id"
        (QC.forAll nonNegativeScientificGen    $ \x -> abs (negate x) === theSci x)
    ]

  , testGroup "Real"
    [ testProperty "fromRational . toRational == id" $ \x ->
        (fromRational . toRational) x === theSci x
    ]

  , testGroup "RealFrac"
    [ testGroup "Equal to Rational"
      [ testProperty "properFraction" $ \x ->
          let (n1::Integer, f1::Scientific) = properFraction x
              (n2::Integer, f2::Rational)   = properFraction (toRational x)
          in (n1 === n2) .&&. (f1 === fromRational f2)

      , testProperty "round" $ \(x::Scientific) ->
          (round x :: Integer) == round (toRational x)

      , testProperty "truncate" $ \(x::Scientific) ->
          (truncate x :: Integer) == truncate (toRational x)

      , testProperty "ceiling" $ \(x::Scientific) ->
          (ceiling x :: Integer) == ceiling (toRational x)

      , testProperty "floor" $ \(x::Scientific) ->
          (floor x :: Integer) == floor (toRational x)
      ]

    , testProperty "properFraction_laws" properFraction_laws

    , testProperty "round"    $ \s -> round    s == roundDefault    s
    , testProperty "truncate" $ \s -> truncate s == truncateDefault s
    , testProperty "ceiling"  $ \s -> ceiling  s == ceilingDefault  s
    , testProperty "floor"    $ \s -> floor    s == floorDefault    s
    ]

  , testGroup "Conversions"
    [ testProperty "fromRationalRepetend" $ \(l, r) -> r ==
        (case fromRationalRepetend (Just l) r of
          Left (s, rr) -> toRational s + rr
          Right (s, mbRepetend) ->
            case mbRepetend of
              Nothing       -> toRational s
              Just repetend -> toRationalRepetend s repetend)

    , testGroup "Float"  $ conversionsProperties (undefined :: Float)
    , testGroup "Double" $ conversionsProperties (undefined :: Double)

    , testGroup "floatingOrInteger"
      [ testProperty "correct conversion" $ \s ->
            case floatingOrInteger s :: Either Double Int of
              Left  d -> d == toRealFloat s
              Right i -> i == fromInteger (coefficient s') * 10^(base10Exponent s')
                  where
                    s' = normalize s
      , testProperty "Integer == Right" $ \(i::Integer) ->
          (floatingOrInteger (fromInteger i) :: Either Double Integer) == Right i
      , testProperty "Double == Left"
          (\(d::Double) -> genericIsFloating d QC.==>
             (floatingOrInteger (realToFrac d) :: Either Double Integer) == Left d)
      ]
    , testGroup "toBoundedInteger"
      [ testGroup "correct conversion"

        [ testCase "100e-2" $ toBoundedInteger @Int (read "100e-2") @?= Just 1
        , testProperty "Int64"       $ toBoundedIntegerConversion (undefined :: Int64)
        , testProperty "Word64"      $ toBoundedIntegerConversion (undefined :: Word64)
        , testProperty "NegativeNum" $ toBoundedIntegerConversion (undefined :: NegativeInt)
        ]
      ]
    ]
  , testGroup "toBoundedRealFloat"
    [ testCase "0 * 10^1000 == 0" $
        toBoundedRealFloat (scientific 0 1000) @?= Right (0 :: Float)
    ]
  , testGroup "toBoundedInteger"
    [ testGroup "to Int64" $
      [ testCase "succ of maxBound" $
        let i = succ . fromIntegral $ (maxBound :: Int64)
            s = scientific i 0
        in (toBoundedInteger s :: Maybe Int64) @?= Nothing
      , testCase "pred of minBound" $
        let i = pred . fromIntegral $ (minBound :: Int64)
            s = scientific i 0
        in (toBoundedInteger s :: Maybe Int64) @?= Nothing
      , testCase "0 * 10^1000 == 0" $
          toBoundedInteger (scientific 0 1000) @?= Just (0 :: Int64)
      ]
    ]
  , testGroup "toBoundedIntegerOrErrorReason"
    [ testGroup "errors"
      [ testCase "127 :: Int8"    $ toBoundedIntegerOrErrorReason @Int8 127 @?= Right 127
      , testCase "128 :: Int8"    $ toBoundedIntegerOrErrorReason @Int8 128 @?= Left TooLarge
      , testCase "1.28e2 :: Int8" $ toBoundedIntegerOrErrorReason @Int8 (read "1.28e2") @?= Left TooLarge
      , testCase "-128 :: Int8"   $ toBoundedIntegerOrErrorReason @Int8 (-128) @?= Right (-128)
      , testCase "-129 :: Int8"   $ toBoundedIntegerOrErrorReason @Int8 (-129) @?= Left TooSmall
      , testCase "1.5 :: Int8"    $ toBoundedIntegerOrErrorReason @Int8 1.5 @?= Left NotAnInteger
      , testCase "1200e-2 :: Int8"  $ toBoundedIntegerOrErrorReason @Int8 (scientific 1200 (-2)) @?= Right 12
      , testCase "12800e-2 :: Int8" $ toBoundedIntegerOrErrorReason @Int8 (scientific 12800 (-2)) @?= Left TooLarge
      , testCase "-12900e-2 :: Int8" $ toBoundedIntegerOrErrorReason @Int8 (scientific (-12900) (-2)) @?= Left TooSmall
      , testCase "15e-1 :: Int8"    $ toBoundedIntegerOrErrorReason @Int8 (scientific 15 (-1)) @?= Left NotAnInteger
        -- Negative exponents with an Int coefficient use the Int division
        -- table, which ends at e = -18 on 64 bit platforms.
      , testCase "12e-1 :: Int"     $ toBoundedIntegerOrErrorReason @Int (scientific 12 (-1)) @?= Left NotAnInteger
      , testCase "1200e-2 :: Int"   $ toBoundedIntegerOrErrorReason @Int (scientific 1200 (-2)) @?= Right 12
      , testCase "-1200e-2 :: Int"  $ toBoundedIntegerOrErrorReason @Int (scientific (-1200) (-2)) @?= Right (-12)
      , testCase "0e-5 :: Int"      $ toBoundedIntegerOrErrorReason @Int (scientific 0 (-5)) @?= Right 0
      , testCase "0e-5 :: NegativeInt" $ toBoundedIntegerOrErrorReason @NegativeInt (scientific 0 (-5)) @?= Left TooLarge
        -- Zero past either end of the scaledInt table.
      , testCase "0e1000 :: Int8"   $ toBoundedIntegerOrErrorReason @Int8 (scientific 0 1000) @?= Right 0
      , testCase "0e1000 :: Word64" $ toBoundedIntegerOrErrorReason @Word64 (scientific 0 1000) @?= Right 0
      , testCase "0e1000 :: Wide"   $ toBoundedIntegerOrErrorReason @Wide (scientific 0 1000) @?= Right 0
      , testCase "0e19 :: Int"      $ toBoundedIntegerOrErrorReason @Int (scientific 0 19) @?= Right 0
      , testCase "0e-19 :: Int"     $ toBoundedIntegerOrErrorReason @Int (scientific 0 (-19)) @?= Right 0
      , testCase "0e-1000 :: Word8" $ toBoundedIntegerOrErrorReason @Word8 (scientific 0 (-1000)) @?= Right 0
      , testCase "0e-1000 :: Wide"  $ toBoundedIntegerOrErrorReason @Wide (scientific 0 (-1000)) @?= Right 0
      , testCase "0e(minBound) :: Int" $ toBoundedIntegerOrErrorReason @Int (scientific 0 minBound) @?= Right 0
      , testCase "isInteger 0e-1000" $ isInteger (scientific 0 (-1000)) @?= True
      , testCase "floatingOrInteger 0e-1000" $ floatingOrInteger @Double @Integer (scientific 0 (-1000)) @?= Right 0
        -- A large coefficient with a negative exponent that normalises to a
        -- value outside the Int range, for a type wider than Int.
      , testCase "2^70 * 10 e-1 :: Wide" $ toBoundedIntegerOrErrorReason @Wide (scientific (2^(70::Int) * 10) (-1)) @?= Right (2^(70::Int))
      , testCase "(2^70 + 1) * 10 e-1 :: Wide" $ toBoundedIntegerOrErrorReason @Wide (scientific ((2^(70::Int) + 1) * 10) (-1)) @?= Left TooLarge
      , testCase "-(2^70) * 10 e-1 :: Wide" $ toBoundedIntegerOrErrorReason @Wide (scientific (-(2^(70::Int)) * 10) (-1)) @?= Right (-(2^(70::Int)))
      , testCase "1e-18 :: Int"     $ toBoundedIntegerOrErrorReason @Int (scientific (10^(18::Int)) (-18)) @?= Right 1
      , testCase "1e-19 :: Int"     $ toBoundedIntegerOrErrorReason @Int (scientific (10^(18::Int)) (-19)) @?= Left NotAnInteger
      , testCase "5e-19 :: Int"     $ toBoundedIntegerOrErrorReason @Int (scientific 5 (-19)) @?= Left NotAnInteger
      , testCase "maxBound Int e-1" $ toBoundedIntegerOrErrorReason @Int (scientific (toInteger (maxBound :: Int)) (-1)) @?= Left NotAnInteger
      , testCase "minBound Int e-1" $ toBoundedIntegerOrErrorReason @Int (scientific (toInteger (minBound :: Int)) (-1)) @?= Left NotAnInteger
      , testCase "-9e18 e-18 :: Int" $ toBoundedIntegerOrErrorReason @Int (scientific (-9 * 10^(18::Int)) (-18)) @?= Right (-9)
      , testCase "128000e-3 :: Int8" $ toBoundedIntegerOrErrorReason @Int8 (scientific 128000 (-3)) @?= Left TooLarge
      , testCase "-1000e-3 :: Word8" $ toBoundedIntegerOrErrorReason @Word8 (scientific (-1000) (-3)) @?= Left TooSmall
        -- Large coefficients with negative exponents still use isInteger_.
      , testCase "10^30 e-20 :: Int"  $ toBoundedIntegerOrErrorReason @Int (scientific (10^(30::Int)) (-20)) @?= Right (10^(10::Int))
      , testCase "10^30 e-5 :: Int"   $ toBoundedIntegerOrErrorReason @Int (scientific (10^(30::Int)) (-5)) @?= Left TooLarge
      , testCase "-10^30 e-5 :: Int"  $ toBoundedIntegerOrErrorReason @Int (scientific (-(10^(30::Int))) (-5)) @?= Left TooSmall
      , testCase "(10^30 + 1) e-20 :: Int" $ toBoundedIntegerOrErrorReason @Int (scientific (10^(30::Int) + 1) (-20)) @?= Left NotAnInteger
      , testCase "2^64 * 10 e-1 :: Word64" $ toBoundedIntegerOrErrorReason @Word64 (scientific (2^(64::Int) * 10 - 10) (-1)) @?= Right maxBound
      , testCase "floatingOrInteger 0e-5" $ floatingOrInteger @Double @Integer (scientific 0 (-5)) @?= Right 0
      , testCase "isInteger 1200e-2"  $ isInteger (scientific 1200 (-2)) @?= True
      , testCase "isInteger 12e-1"    $ isInteger (scientific 12 (-1)) @?= False
      , testCase "-1 :: Word8"    $ toBoundedIntegerOrErrorReason @Word8 (-1) @?= Left TooSmall
      , testCase "-1.5 :: Word8"  $ toBoundedIntegerOrErrorReason @Word8 (-1.5) @?= Left NotAnInteger
      , testCase "0 :: NegativeInt" $ toBoundedIntegerOrErrorReason @NegativeInt 0 @?= Left TooLarge
      , testCase "0e1000 :: NegativeInt" $ toBoundedIntegerOrErrorReason @NegativeInt (scientific 0 1000) @?= Left TooLarge
      , testCase "-5e1 :: NegativeInt" $ toBoundedIntegerOrErrorReason @NegativeInt (scientific (-5) 1) @?= Right (-50)
      , testCase "1001 :: Narrow" $ toBoundedIntegerOrErrorReason @Narrow 1001 @?= Left TooLarge
      , testCase "-1001 :: Narrow" $ toBoundedIntegerOrErrorReason @Narrow (-1001) @?= Left TooSmall
      , testCase "1e3 :: Narrow" $ toBoundedIntegerOrErrorReason @Narrow (scientific 1 3) @?= Right 1000
      , testCase "1e30 :: Narrow" $ toBoundedIntegerOrErrorReason @Narrow (scientific 1 30) @?= Left TooLarge
      , testCase "2^64 :: Wide" $ toBoundedIntegerOrErrorReason @Wide (2^(64::Int)) @?= Right (2^(64::Int))
      , testCase "-2^64 :: Wide" $ toBoundedIntegerOrErrorReason @Wide (-(2^(64::Int))) @?= Right (-(2^(64::Int)))
      , testCase "1e20 :: Wide" $ toBoundedIntegerOrErrorReason @Wide (scientific 1 20) @?= Right (10^(20::Int))
      , testCase "1e30 :: Wide" $ toBoundedIntegerOrErrorReason @Wide (scientific 1 30) @?= Left TooLarge
      , testCase "-1e30 :: Wide" $ toBoundedIntegerOrErrorReason @Wide (scientific (-1) 30) @?= Left TooSmall
        -- The exponent check that skips the slow path: boundsLog is 21 for
        -- Wide (2^70 ~ 1.18e21) and 19 for Word64 (2^64 - 1 ~ 1.8e19).
      , testCase "1e21 :: Wide"    $ toBoundedIntegerOrErrorReason @Wide (scientific 1 21) @?= Right (10^(21::Int))
      , testCase "-1e21 :: Wide"   $ toBoundedIntegerOrErrorReason @Wide (scientific (-1) 21) @?= Right (-(10^(21::Int)))
      , testCase "12e20 :: Wide"   $ toBoundedIntegerOrErrorReason @Wide (scientific 12 20) @?= Left TooLarge
      , testCase "1e22 :: Wide"    $ toBoundedIntegerOrErrorReason @Wide (scientific 1 22) @?= Left TooLarge
      , testCase "-1e22 :: Wide"   $ toBoundedIntegerOrErrorReason @Wide (scientific (-1) 22) @?= Left TooSmall
      , testCase "1e19 :: Word64"  $ toBoundedIntegerOrErrorReason @Word64 (scientific 1 19) @?= Right (10^(19::Int))
      , testCase "2e19 :: Word64"  $ toBoundedIntegerOrErrorReason @Word64 (scientific 2 19) @?= Left TooLarge
      , testCase "1e20 :: Word64"  $ toBoundedIntegerOrErrorReason @Word64 (scientific 1 20) @?= Left TooLarge
      , testCase "1e500 :: Word64" $ toBoundedIntegerOrErrorReason @Word64 (scientific 1 500) @?= Left TooLarge
      , testCase "-1e500 :: Word64" $ toBoundedIntegerOrErrorReason @Word64 (scientific (-1) 500) @?= Left TooSmall
      , testCase "5 :: AboveInt" $ toBoundedIntegerOrErrorReason @AboveInt 5 @?= Left TooSmall
      , testCase "0 :: AboveInt" $ toBoundedIntegerOrErrorReason @AboveInt 0 @?= Left TooSmall
      , testCase "2^70 :: AboveInt" $ toBoundedIntegerOrErrorReason @AboveInt (2^(70::Int)) @?= Right (2^(70::Int))
      , testCase "1e30 :: AboveInt" $ toBoundedIntegerOrErrorReason @AboveInt (scientific 1 30) @?= Left TooLarge
      , testCase "0 * 10^1000"    $ toBoundedIntegerOrErrorReason @Int8 (scientific 0 1000) @?= Right 0
      , testCase "1e1000000000"   $ toBoundedIntegerOrErrorReason @Int (read "1e1000000000") @?= Left TooLarge
      , testCase "-1e1000000000"  $ toBoundedIntegerOrErrorReason @Int (read "-1e1000000000") @?= Left TooSmall
      , testCase "1e-1000000000"  $ toBoundedIntegerOrErrorReason @Int (read "1e-1000000000") @?= Left NotAnInteger
      , testCase "2^31 :: Word32"     $ toBoundedIntegerOrErrorReason @Word32 (2^(31::Int)) @?= Right (2^(31::Int))
      , testCase "2^32 - 1 :: Word32" $ toBoundedIntegerOrErrorReason @Word32 (2^(32::Int) - 1) @?= Right maxBound
      , testCase "2^32 :: Word32"     $ toBoundedIntegerOrErrorReason @Word32 (2^(32::Int)) @?= Left TooLarge
      , testCase "-2^63 - 1 :: Int"   $ toBoundedIntegerOrErrorReason @Int (-(2^(63::Int)) - 1) @?= Left TooSmall
      , testCase "2^64 :: Word"       $ toBoundedIntegerOrErrorReason @Word (2^(64::Int)) @?= Left TooLarge
      , testCase "maxBound Word64" $ toBoundedIntegerOrErrorReason @Word64 (fromIntegral (maxBound :: Word64)) @?= Right maxBound
      , testCase "minBound Int64"  $ toBoundedIntegerOrErrorReason @Int64 (fromIntegral (minBound :: Int64)) @?= Right minBound
      , testCase "2^63 - 1 :: Word64"     $ toBoundedIntegerOrErrorReason @Word64 (2^(63::Int) - 1) @?= Right (2^(63::Int) - 1)
      , testCase "922337203685477581e1 :: Word64" $
          toBoundedIntegerOrErrorReason @Word64 (scientific 922337203685477581 1) @?= Right 9223372036854775810
      , testCase "1844674407370955162e1 :: Word64" $
          toBoundedIntegerOrErrorReason @Word64 (scientific 1844674407370955162 1) @?= Left TooLarge
      , testCase "2^63 :: Word64"         $ toBoundedIntegerOrErrorReason @Word64 (2^(63::Int)) @?= Right (2^(63::Int))
      , testCase "2^64 - 1 :: Word64"     $ toBoundedIntegerOrErrorReason @Word64 (2^(64::Int) - 1) @?= Right maxBound
      , testCase "2^64 :: Word64"         $ toBoundedIntegerOrErrorReason @Word64 (2^(64::Int)) @?= Left TooLarge
      , testCase "2^63 * 10 :: Word64"    $ toBoundedIntegerOrErrorReason @Word64 (scientific (2^(63::Int)) 1) @?= Left TooLarge
      , testCase "-2^63 :: Word64"        $ toBoundedIntegerOrErrorReason @Word64 (-(2^(63::Int))) @?= Left TooSmall
      , testCase "-2^63 - 1 :: Word64"    $ toBoundedIntegerOrErrorReason @Word64 (-(2^(63::Int)) - 1) @?= Left TooSmall
      , testCase "2^63 :: Int64"          $ toBoundedIntegerOrErrorReason @Int64 (2^(63::Int)) @?= Left TooLarge
      , testCase "-2^63 - 1 :: Int64"     $ toBoundedIntegerOrErrorReason @Int64 (-(2^(63::Int)) - 1) @?= Left TooSmall
      -- Coefficients from 2^31 to 2^63 take the big coefficient path on 32
      -- bit platforms.
      , testCase "3000000000 :: Int64"      $ toBoundedIntegerOrErrorReason @Int64 3000000000 @?= Right 3000000000
      , testCase "-3000000000 :: Int64"     $ toBoundedIntegerOrErrorReason @Int64 (-3000000000) @?= Right (-3000000000)
      , testCase "3000000000e9 :: Int64"    $ toBoundedIntegerOrErrorReason @Int64 (scientific 3000000000 9) @?= Right 3000000000000000000
      , testCase "9300000000e9 :: Int64"    $ toBoundedIntegerOrErrorReason @Int64 (scientific 9300000000 9) @?= Left TooLarge
      , testCase "-9300000000e9 :: Int64"   $ toBoundedIntegerOrErrorReason @Int64 (scientific (-9300000000) 9) @?= Left TooSmall
      , testCase "3000000000e10 :: Int64"   $ toBoundedIntegerOrErrorReason @Int64 (scientific 3000000000 10) @?= Left TooLarge
      , testCase "3000000000 :: Word64"     $ toBoundedIntegerOrErrorReason @Word64 3000000000 @?= Right 3000000000
      , testCase "18446744073e9 :: Word64"  $ toBoundedIntegerOrErrorReason @Word64 (scientific 18446744073 9) @?= Right 18446744073000000000
      , testCase "18446744074e9 :: Word64"  $ toBoundedIntegerOrErrorReason @Word64 (scientific 18446744074 9) @?= Left TooLarge
      , testCase "-3000000000 :: Word64"    $ toBoundedIntegerOrErrorReason @Word64 (-3000000000) @?= Left TooSmall
#if WORD_SIZE_IN_BITS == 64
      , testCase "2^64 - 1 :: Word"       $ toBoundedIntegerOrErrorReason @Word (2^(64::Int) - 1) @?= Right maxBound
#endif
      ]
    , testGroup "agrees with reference"
      [ testGroup "Int"
        [ testProperty "random" $ boundedIntegerProp (undefined :: Int)
        , testCase "boundaries" $ boundedBoundaries (undefined :: Int)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe Int)
        ]
      , testGroup "Int8"
        [ testProperty "random" $ boundedIntegerProp (undefined :: Int8)
        , testCase "boundaries" $ boundedBoundaries (undefined :: Int8)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe Int8)
        ]
      , testGroup "Int16"
        [ testProperty "random" $ boundedIntegerProp (undefined :: Int16)
        , testCase "boundaries" $ boundedBoundaries (undefined :: Int16)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe Int16)
        ]
      , testGroup "Int32"
        [ testProperty "random" $ boundedIntegerProp (undefined :: Int32)
        , testCase "boundaries" $ boundedBoundaries (undefined :: Int32)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe Int32)
        ]
      , testGroup "Int64"
        [ testProperty "random" $ boundedIntegerProp (undefined :: Int64)
        , testCase "boundaries" $ boundedBoundaries (undefined :: Int64)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe Int64)
        ]
      , testGroup "Word"
        [ testProperty "random" $ boundedIntegerProp (undefined :: Word)
        , testCase "boundaries" $ boundedBoundaries (undefined :: Word)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe Word)
        ]
      , testGroup "Word8"
        [ testProperty "random" $ boundedIntegerProp (undefined :: Word8)
        , testCase "boundaries" $ boundedBoundaries (undefined :: Word8)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe Word8)
        ]
      , testGroup "Word16"
        [ testProperty "random" $ boundedIntegerProp (undefined :: Word16)
        , testCase "boundaries" $ boundedBoundaries (undefined :: Word16)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe Word16)
        ]
      , testGroup "Word32"
        [ testProperty "random" $ boundedIntegerProp (undefined :: Word32)
        , testCase "boundaries" $ boundedBoundaries (undefined :: Word32)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe Word32)
        ]
      , testGroup "Word64"
        [ testProperty "random" $ boundedIntegerProp (undefined :: Word64)
        , testCase "boundaries" $ boundedBoundaries (undefined :: Word64)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe Word64)
        ]
      , testGroup "NegativeInt"
        [ testProperty "random" $ boundedIntegerProp (undefined :: NegativeInt)
        , testCase "boundaries" $ boundedBoundaries (undefined :: NegativeInt)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe NegativeInt)
        ]
      , testGroup "Narrow"
        [ testProperty "random" $ boundedIntegerProp (undefined :: Narrow)
        , testCase "boundaries" $ boundedBoundaries (undefined :: Narrow)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe Narrow)
        ]
      , testGroup "Wide"
        [ testProperty "random" $ boundedIntegerProp (undefined :: Wide)
        , testCase "boundaries" $ boundedBoundaries (undefined :: Wide)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe Wide)
        ]
      , testGroup "AboveInt"
        [ testProperty "random" $ boundedIntegerProp (undefined :: AboveInt)
        , testCase "boundaries" $ boundedBoundaries (undefined :: AboveInt)
        , testProperty "toBoundedInteger matches reference" $ \s ->
            toBoundedInteger s === (either (const Nothing) Just (boundedIntegerReference s) :: Maybe AboveInt)
        ]
      ]
    ]
  , testGroup "Predicates"
    [ testProperty "isFloating" $ \s -> isFloating s ==      genericIsFloating s
    , testProperty "isInteger"  $ \s -> isInteger  s == not (genericIsFloating s)
    ]
  ]

-- used as type annotation
theSci :: Scientific -> Scientific
theSci = id

testMain :: TestTree -> IO ()
testMain = defaultMainWithIngredients defaultIngredients

testReads :: String -> [(Scientific, String)] -> Assertion
testReads inp out = reads inp @?= out

testRead :: String -> Scientific -> Assertion
testRead inp out = read inp @?= out

testShow :: Maybe Scientific -> String -> Assertion
testShow inp out = show inp @?= out

testScientificP :: String -> [(Scientific, String)] -> Assertion
testScientificP inp out = readP_to_S Scientific.scientificP inp @?= out

genericIsFloating :: RealFrac a => a -> Bool
genericIsFloating a = fromInteger (floor a :: Integer) /= a

toDecimalDigits_eq_floatToDigits :: Double -> Bool
toDecimalDigits_eq_floatToDigits d =
    Scientific.toDecimalDigits (Scientific.fromFloatDigits d)
      == Numeric.floatToDigits 10 d

conversionsProperties :: forall realFloat.
                         ( RealFloat    realFloat
                         , QC.Arbitrary realFloat
                         , Show         realFloat
                         )
                      => realFloat -> [TestTree]
conversionsProperties _ =
  [
    -- testProperty "fromFloatDigits_1" $ \(d :: realFloat) ->
    --   Scientific.fromFloatDigits d === realToFrac d

    -- testProperty "fromFloatDigits_2" $ \(s :: Scientific) ->
    --   Scientific.fromFloatDigits (realToFrac s :: realFloat) == s

    testProperty "toRealFloat" $ \(d :: realFloat) ->
      (Scientific.toRealFloat . realToFrac) d == d

  , testProperty "toRealFloat . fromFloatDigits == id" $ \(d :: realFloat) ->
      (Scientific.toRealFloat . Scientific.fromFloatDigits) d == d

  -- , testProperty "fromFloatDigits . toRealFloat == id" $ \(s :: Scientific) ->
  --     Scientific.fromFloatDigits (Scientific.toRealFloat s :: realFloat) == s
  ]

toBoundedIntegerConversion
    :: forall i. (Integral i, Bounded i)
    => i -> Scientific -> Bool
toBoundedIntegerConversion _ s =
    case toBoundedInteger s :: Maybe i of
      Just i -> i == (fromIntegral $ (coefficient s') * 10^(base10Exponent s')) &&
                i >= minBound &&
                i <= maxBound
        where
          s' = normalize s
      Nothing -> isFloating s ||
                 s < fromIntegral (minBound :: i) ||
                 s > fromIntegral (maxBound :: i)

-- | Reference implementation of 'toBoundedIntegerOrErrorReason'.
boundedIntegerReference
    :: forall i. (Integral i, Bounded i)
    => Scientific -> Either ToBoundedIntegerError i
boundedIntegerReference s
    | isFloating s = Left NotAnInteger
    | s < fromIntegral (minBound :: i) = Left TooSmall
    | s > fromIntegral (maxBound :: i) = Left TooLarge
    | otherwise = case floatingOrInteger s :: Either Double Integer of
        Right n -> Right (fromInteger n)
        Left _  -> Left NotAnInteger

boundedIntegerProp
    :: forall i. (Integral i, Bounded i, Show i)
    => i -> Scientific -> Property
boundedIntegerProp _ s =
    toBoundedIntegerOrErrorReason s === (boundedIntegerReference s :: Either ToBoundedIntegerError i)

-- | Checks every table entry: for each exponent, the coefficients around
-- @bound `quot` 10^e@ must be classified as the reference does.
boundedBoundaries
    :: forall i. (Integral i, Bounded i, Show i)
    => i -> Assertion
boundedBoundaries _ = do
    forM_ [0 .. 25] $ \e ->
      forM_ [toInteger (minBound :: i), toInteger (maxBound :: i)] $ \bound ->
        forM_ [-2 .. 2] $ \d ->
          check (scientific (bound `quot` 10 ^ e + d) e)
    -- Coefficients around the edges of the Int range, where the conversion
    -- switches between the small and big coefficient paths.
    forM_ [0 .. 2] $ \e ->
      forM_ [toInteger (minBound :: Int), toInteger (maxBound :: Int)] $ \bound ->
        forM_ [-2 .. 2] $ \d ->
          check (scientific (bound + d) e)
  where
    check s =
      toBoundedIntegerOrErrorReason s @?= (boundedIntegerReference s :: Either ToBoundedIntegerError i)

bin :: (forall a. Num a => a -> a -> a) -> Scientific -> Scientific -> Bool
bin op a b = toRational (a `op` b) == toRational a `op` toRational b

unary :: (forall a. Num a => a -> a) -> Scientific -> Bool
unary op a = toRational (op a) == op (toRational a)

toDecimalDigits_laws :: Scientific -> Bool
toDecimalDigits_laws x =
  let (ds, e) = Scientific.toDecimalDigits x

      rule1 = n >= 1
      n     = length ds

      rule2 = toRational x == coeff * 10 ^^ e
      coeff = foldr (\di a -> a / 10 + fromIntegral di) 0 (0:ds)

      rule3 = all (\di -> 0 <= di && di <= 9) ds

      rule4 | n == 1    = True
            | otherwise = null $ takeWhile (==0) $ reverse ds

  in rule1 && rule2 && rule3 && rule4

properFraction_laws :: Scientific -> Property
properFraction_laws x = fromInteger n + f === x        .&&.
                        (positive n == posX || n == 0) .&&.
                        (positive f == posX || f == 0) .&&.
                        abs f < 1
    where
      posX = positive x

      (n, f) = properFraction x :: (Integer, Scientific)

positive :: (Ord a, Num a) => a -> Bool
positive y = y >= 0

floorDefault :: Scientific -> Integer
floorDefault x = if r < 0 then n - 1 else n
                 where (n,r) = properFraction x

ceilingDefault :: Scientific -> Integer
ceilingDefault x = if r > 0 then n + 1 else n
                   where (n,r) = properFraction x

truncateDefault :: Scientific -> Integer
truncateDefault x =  m where (m,_) = properFraction x

roundDefault :: Scientific -> Integer
roundDefault x = let (n,r) = properFraction x
                     m     = if r < 0 then n - 1 else n + 1
                 in case signum (abs r - 0.5) of
                      -1 -> n
                      0  -> if even n then n else m
                      1  -> m
                      _  -> error "round default defn: Bad value"

newtype NegativeInt = NegativeInt Int
    deriving (Show, Enum, Eq, Ord, Num, Real, Integral)

instance Bounded NegativeInt where
    minBound = -100
    maxBound = -10

-- | Bounds that aren't powers of ten, to test the clamped bounds checks.
newtype Narrow = Narrow Int
    deriving (Show, Enum, Eq, Ord, Num, Real, Integral)

instance Bounded Narrow where
    minBound = -1000
    maxBound = 1000

-- | Bounds wider than 'Int' on both sides, so values outside the 'Int' range
-- take the slow path.
newtype Wide = Wide Integer
    deriving (Show, Enum, Eq, Ord, Num, Real, Integral)

instance Bounded Wide where
    minBound = -(2 ^ (70 :: Int))
    maxBound = 2 ^ (70 :: Int)

-- | A range entirely above the 'Int' range, so every input takes the slow
-- path.
newtype AboveInt = AboveInt Integer
    deriving (Show, Enum, Eq, Ord, Num, Real, Integral)

instance Bounded AboveInt where
    minBound = 2 ^ (70 :: Int)
    maxBound = 2 ^ (71 :: Int)

----------------------------------------------------------------------
-- QuickCheck instances
----------------------------------------------------------------------

instance QC.Arbitrary Scientific where
    arbitrary = QC.frequency
      [ (70, scientific <$> QC.arbitrary
                        <*> intGen)
      , (20, scientific <$> QC.arbitrary
                        <*> bigIntGen)
      , (10, scientific <$> pure 0
                        <*> bigIntGen)
      , (10, (\c e' e -> scientific (c * 10 ^ min 10 (abs e')) e) <$> QC.arbitrary <*> intGen <*> intGen)
      ]

    shrink s =
        [ scientific c e
        | (c, e) <- QC.shrink (Scientific.coefficient s, Scientific.base10Exponent s)
        ]

nonNegativeScientificGen :: QC.Gen Scientific
nonNegativeScientificGen =
    scientific <$> (QC.getNonNegative <$> QC.arbitrary)
               <*> intGen

normalizedScientificGen :: QC.Gen Scientific
normalizedScientificGen = Scientific.normalize <$> QC.arbitrary

bigIntGen :: QC.Gen Int
bigIntGen = QC.sized $ \size -> QC.resize (size * 1000) intGen

intGen :: QC.Gen Int
intGen = QC.arbitrary
