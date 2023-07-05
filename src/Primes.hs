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
)
where

import Math.NumberTheory.ArithmeticFunctions ()
import Math.NumberTheory.Primes
import Data.Bits

nearestPrime :: (Integral a, Data.Bits.Bits a, UniqueFactorisation a) => a -> Prime a
nearestPrime n 
      | abs (unPrime (nextPrime n) - n) < abs (unPrime (precPrime n) - n)     = nextPrime n
      | otherwise                                                             = precPrime n