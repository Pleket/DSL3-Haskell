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

isValidPrime   :: (Bits a, Integral a, UniqueFactorisation a) => a   -- ^ The number to check if it is a prime, with the type constraints of a
               -> Bool                                               -- ^ True if the number is a prime, False otherwise
isValidPrime x = isJust (isPrime x)

showFactors    :: (UniqueFactorisation a, Ord a, Show a) => [(Prime a, Word)]    -- ^ The list of primes and their exponents
               -> String                                                         -- ^ The string representation of the list of primes and their exponents
showFactors n = intercalate " * "
  [ show (unPrime b) <> "^" <> show e
  | (b, e) <- sort n
  ]

getNextPrime   :: (Bits a, Integral a, UniqueFactorisation a) => a   -- ^ The number to find the next prime for, with the type constraints of a
               -> Prime a                                            -- ^ The next prime, wrapped in Prime
getNextPrime n    | isValidPrime n     = nextPrime (n + 1)
                  | otherwise          = nextPrime n

getPrevPrime   :: (Bits a, Integral a, UniqueFactorisation a) => a   -- ^ The number to find the previous prime for, with the type constraints of a
               -> Prime a                                            -- ^ The previous prime, wrapped in Prime
getPrevPrime 2    = precPrime 2
getPrevPrime n    | isValidPrime n     = precPrime (n - 1)
                  | otherwise          = precPrime n

getNearestPrime   :: (Bits a, Integral a, UniqueFactorisation a) => a   -- ^ The number to find the nearest prime for, with the type constraints of a
                  -> Prime a                                            -- ^ The nearest prime, wrapped in Prime
getNearestPrime 1    = nextPrime 2
getNearestPrime n    | isValidPrime n     = findNearest (getNextPrime (n + 1)) (getPrevPrime (n - 1)) (nextPrime n)
                     | otherwise          = nearestPrime n
   where
      findNearest :: (Bits a, Integral a, UniqueFactorisation a) => Prime a   -- ^ The next prime number, wrapped in Prime
                  -> Prime a                                                  -- ^ The previous prime number, wrapped in Prime
                  -> Prime a                                                  -- ^ The current prime number we want to find the nearest prime for, wrapped in Prime
                  -> Prime a                                                  -- ^ The nearest prime number, wrapped in Prime
      findNearest a b x    | (unPrime a + unPrime b) `div` 2 > unPrime x   = b
                           | otherwise                                     = a

-- | Returns the nearest prime, assuming the input is not prime
nearestPrime   :: (Integral a, Bits a, UniqueFactorisation a) => a   -- ^ The number to find the nearest prime for, with the type constraints of a and not a prime itself
               -> Prime a                                            -- ^ The nearest prime, wrapped in Prime 
nearestPrime n
      | abs (unPrime (nextPrime n) - n) < abs (unPrime (precPrime n) - n)     = nextPrime n
      | otherwise                                                             = precPrime n