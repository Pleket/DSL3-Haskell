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

nearestPrime :: (Integral a, Bits a, UniqueFactorisation a) => a -> Prime a
nearestPrime n 
      | abs (unPrime (nextPrime n) - n) < abs (unPrime (precPrime n) - n)     = nextPrime n
      | otherwise                                                             = precPrime n