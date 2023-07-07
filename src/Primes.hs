{-|
   Module       : Primes
   Description  : A module describing basic number theory in Haskell
   Copyright    : (c) Marco Pleket (1295713)
                    Timon Heuwekemeijer
   License      : None
-}
module Primes (
   Prime, UniqueFactorisation, Word
   , isPrime, factorise, factorBack
   , unPrime, nextPrime, precPrime, nearestPrime
   , primeCount, nthPrime, approxPrimeCount, nthPrimeApprox

   , isValidPrime, showFactors, getNextPrime, getPrevPrime, getNearestPrime

   , PrimeIntSet, IntSet
   , singleton, fromList
   , insert, delete, member, notMember, size, isSubsetOf, disjoint
   , lookupEQ, lookupLT, lookupGT, lookupLE, lookupGE
)
where

import Math.NumberTheory.ArithmeticFunctions ()
import Math.NumberTheory.Primes
import Data.Bits (Bits)
import Math.NumberTheory.Primes.Counting
    ( primeCount, nthPrime, approxPrimeCount, nthPrimeApprox )
import Math.NumberTheory.Primes.IntSet
import Data.IntSet (IntSet)
import Data.Maybe
import Data.List (sort, intercalate)

nearestPrime :: (Integral a, Bits a, UniqueFactorisation a) => a -> Prime a
nearestPrime n
      | abs (unPrime (nextPrime n) - n) < abs (unPrime (precPrime n) - n)     = nextPrime n
      | otherwise                                                             = precPrime n

isValidPrime :: (Bits a, Integral a, UniqueFactorisation a) => a -> Bool
isValidPrime x = isJust (isPrime x)

showFactors :: (UniqueFactorisation a, Ord a, Show a) => [(Prime a, Word)] -> String
showFactors n = intercalate " * "
  [ show (unPrime b) <> "^" <> show e
  | (b, e) <- sort n
  ]

getNextPrime :: (Bits a, Integral a, UniqueFactorisation a) => a -> Prime a
getNextPrime n    | isValidPrime n     = nextPrime (n + 1)
                  | otherwise          = nextPrime n

getPrevPrime :: (Bits a, Integral a, UniqueFactorisation a) => a -> Prime a
getPrevPrime 1    = precPrime 1
getPrevPrime n    | isValidPrime n     = precPrime (n - 1)
                  | otherwise          = precPrime n

getNearestPrime :: (Bits a, Integral a, UniqueFactorisation a) => a -> Prime a
getNearestPrime 1    = nextPrime 2
getNearestPrime n    | isValidPrime n     = findNearest (getNextPrime (n + 1)) (getPrevPrime (n - 1)) (nextPrime n)
                     | otherwise          = nearestPrime n
   where
      findNearest :: (Bits a, Integral a, UniqueFactorisation a) => Prime a -> Prime a -> Prime a -> Prime a
      findNearest a b x    | (unPrime a + unPrime b) `div` 2 > unPrime x  = b
                           | otherwise                = a