{-|
   Module       : Primes
   Description  : A module describing basic number theory in Haskell
   Copyright    : (c) Marco Pleket (1295713)
                    Timon Heuwekemeijer
   License      : None
-}
module Primes (
   Prime,
   isPrime, factorise, factorBack
   , unPrime, nextPrime, precPrime, nearestPrime
)
where

import Math.NumberTheory.ArithmeticFunctions ()
import Math.NumberTheory.Primes

nearestPrime :: Integral a => a -> Prime a
nearestPrime n 
      | abs (nextPrime n - n) < abs (precPrime n - n)    = nextPrime n
      | otherwise                                        = precPrime n