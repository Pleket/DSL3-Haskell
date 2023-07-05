{-|
   Module       : Moduli
   Description  : A module describing basic moduli for number theory in Haskell
   Copyright    : (c) Marco Pleket (1295713)
                    Timon Heuwekemeijer
   License      : None
-}
module Moduli (
   Mod
   , getVal, getMod
   , invertMod, Math.NumberTheory.Powers.Modular.powMod, powModWord, powModInt
   , MultMod
   , isMultElement, multElement, invertGroup
   , chinese, solveLinear, solveQuadratic
   , sqrtsMod, sqrtsModFactorisation, sqrtsModPrimePower, sqrtsModPrime
   , PrimitiveRoot
   , unPrimitiveRoot, isPrimitiveRoot, discreteLogarithm
   , JacobiSymbol
   , jacobi

) where

import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.Chinese
import Math.NumberTheory.Moduli.Equations
import Math.NumberTheory.Moduli.Multiplicative
import Math.NumberTheory.Moduli.Sqrt

import Math.NumberTheory.Powers.Modular