{-|
   Module       : Algebra
   Description  : A module describing polynomial algebra in Haskell
   Copyright    : (c) Marco Pleket (1295713)
                    Timon Heuwekemeijer
   License      : None
-}
module Algebra (
    Poly, PolyRemainder
    , showPoly
    , greaterPoly, equalPoly
    , addPoly, subtractPoly, multPoly, expPoly
    , multPolyVar, modDivPoly, modDivPolyRemainder, gcdPoly
    , solution
)
where

-- import Data.List
import GHC.Natural (Natural)
import Debug.Trace

type Poly = [Int]
type PolyRemainder = (Poly, Poly)

showPoly :: Poly -> String
showPoly xs = showExpr xs 0 True

-- | 'showExpr' takes a polynomial and returns a string representation of it. 
showExpr    :: Poly     -- ^ The polynomial to be converted to a string.
            -> Int      -- ^ The current constant value. 
            -> Bool     -- ^ Whether or not the current value is the first (visible) value in the polynomial.
            -> String   -- ^ The string representation of the polynomial.
showExpr [] _ _ = ""
showExpr (x:xs) i first_val
    | x == 0                = showExpr xs (i+1) first_val
    | i == 0 && x /= 0      = show x ++ showExpr xs (i+1) False
    | i == 1 && x /= 0      = addOrSubt x first_val ++ show (abs x) ++ "x" ++ showExpr xs (i+1) False
    | x == 1 || x == -1     = addOrSubt x first_val ++ "x^" ++ show i ++ showExpr xs (i+1) False
    | otherwise             = addOrSubt x first_val ++ show (abs x) ++ "x^" ++ show i ++ showExpr xs (i+1) False

-- | 'addOrSubt' is used for extra formatting on additions and subtractions in the showExpr function. 
addOrSubt   :: Int      -- ^ The current value being formatted.
            -> Bool     -- ^ Whether or not the current value is the first (visible) value in the polynomial.
            -> String   -- ^ The formatted string placed before every x.
addOrSubt x first_val
    | not first_val && x > 0    = " + "
    | not first_val && x < 0    = " - "
    | first_val && x < 0        = "-"
    | otherwise                 = ""


-- | 'greaterPoly' takes two polynomials and returns whether or not the first polynomial is greater than the second polynomial.
greaterPoly     :: Poly
                -> Poly
                -> Bool     -- ^ Whether or not the first polynomial is greater than the second polynomial.
greaterPoly [] [] = False
greaterPoly xs ys   | length xs > length ys     = True
                    | length xs < length ys     = False
                    | otherwise                 = checkGreater (zip (reverse xs) (reverse ys))
    where
        checkGreater :: [(Int, Int)] -> Bool
        checkGreater [] = False
        checkGreater ((a, b):as)
            | a > b     = True
            | a < b     = False
            | otherwise = checkGreater as

-- | 'equalPoly' takes two polynomials and returns whether or not they are equal.
equalPoly   :: Poly
            -> Poly
            -> Bool     -- ^ Whether or not the two polynomials are equal.
equalPoly xs ys = xs == ys

-- | 'addPoly' takes two polynomials and returns the addition of them, without trailing zeros. 
addPoly     :: Poly
            -> Poly
            -> Poly
addPoly xs ys = cutZeros (addPolyAux xs ys)
-- | 'subtractPoly' takes two polynomials and returns the subtraction of them, without trailing zeros.
subtractPoly    :: Poly
                -> Poly
                -> Poly
subtractPoly xs [] = xs
subtractPoly xs ys = addPoly xs (map (* (-1)) ys)

-- | 'addPolyAux' takes two polynomials and returns the addition of them.
addPolyAux  :: Poly         -- ^ The first polynomial.
            -> Poly         -- ^ The second polynomial.
            -> Poly         -- ^ The result of adding two polynomials.
addPolyAux xs [] = xs
addPolyAux [] ys = ys
addPolyAux (x:xs) (y:ys) = (x+y) : addPolyAux xs ys

-- | 'multPoly' takes two polynomials and returns the multiplication of them, without trailing zeros.
multPoly    :: Poly
            -> Poly
            -> Poly
multPoly xs ys = cutZeros (multPolyAux xs ys)

-- | 'multPolyAux' takes two polynomials and returns the multiplication of them.
multPolyAux     :: Poly     -- ^ The first polynomial.
                -> Poly     -- ^ The second polynomial.
                -> Poly     -- ^ The result of multiplying two polynomials, whose length is <= the sum of the lengths of the two polynomials.
multPolyAux [] _ = []
multPolyAux _ [] = []
multPolyAux (x:xs) ys = addPolyAux (map (* x) ys) (multPolyAux xs (0 : ys))


-- | 'exp' takes a polynomial and a natural number and returns the polynomial to the power of the natural number.
expPoly     :: Poly     -- ^ The polynomial to be raised to the power of the natural number.
            -> Natural  -- ^ The exponent as a natural number (roots are not yet supported)
            -> Poly     -- ^ The result of the polynomial to the power of the natural number.
expPoly _ 0 = [1]
expPoly [] _ = []
expPoly xs n = fastExp xs n xs

-- | 'exp' takes a polynomial and a natural number and returns the polynomial to the power of the natural number.
fastExp         :: Poly     -- ^ The polynomial to be raised to the power of the natural number.
                -> Natural  -- ^ The exponent as a natural number (roots are not yet supported)
                -> Poly     -- ^ The polynomial that keeps track of the fast exponentiation
                -> Poly     -- ^ The result of the polynomial to the power of the natural number.
fastExp _ 1 ys = ys
fastExp ys i zs     | even i            = fastExp ys (i `div` 2) (multPoly zs zs)
                    | otherwise         = fastExp ys (i `div` 2) (multPoly ys (multPoly zs zs))

-- | 'cutZeros' takes a polynomial and returns the same polynomial without the trailing zeros.
cutZeros    :: Poly     -- ^ The polynomial to be cut.
            -> Poly     -- ^ The polynomial without trailing zeros.
cutZeros [] = [0]
cutZeros xs = reverse (cutZeros' (reverse xs))
    where
        cutZeros' :: Poly -> Poly
        cutZeros' [] = [0]
        cutZeros' (y:ys)
            | y == 0    = cutZeros' ys
            | otherwise = y : ys

-- | 'modDivPoly' takes two polynomials and returns the floored division of them. 
modDivPoly      :: Poly
                -> Poly
                -> Poly
modDivPoly xs ys 
    | greaterPoly xs ys         = cutZeros (reverse (modDivPolyAux xs ys))
    | otherwise                 = [0]

-- | 'modDivPolyAux' takes two polynomials and returns the floored division of them, in reversed poly order with trailing zeros. 
modDivPolyAux   :: Poly
                -> Poly
                -> Poly
modDivPolyAux xs ys     = findDiv (reverse xs) (reverse ys) (length xs - length ys)
        where 
            findDiv     :: Poly     -- ^ The polynomial to be divided.
                        -> Poly     -- ^ The polynomial to divide by (multiplied by the variable x to the power of i to start at the highest digit).
                        -> Int      -- ^ The current power of the variable x that the second input is multiplied by.
                        -> Poly
            findDiv [] _ _ = []
            findDiv (a:_) (b:_) 0 = [a `div` b]
            findDiv (a:as) (b:bs) i
                | a == 0            = 0 : findDiv as (b:bs) (i-1)
                | a `div` b == 0    = 0 : findDiv as (b:bs) (i-1)
                | otherwise         = a `div` b : 
                        findDiv (
                            reverse (addPolyAux
                                (reverse as)
                                (map (* (-1)) (multPoly (multPolyVar (reverse bs) i) [a `div` b]))
                            )) (b:bs) (i-1)

-- | 'modDivPolyRemainder' takes two polynomials and returns the remainder of the floored division of them.
modDivPolyRemainder :: Poly     -- ^ The polynomial to be divided.
                    -> Poly     -- ^ The polynomial to divide by.
                    -> Poly     -- ^ The remainder of the floored division of the two polynomials.
modDivPolyRemainder xs ys = subtractPoly xs (multPoly ys (modDivPoly xs ys))

-- | 'multPolyVar' takes a polynomial and an integer and returns the polynomial multiplied by the variable to the power of the integer.
multPolyVar     :: Poly     -- ^ The polynomial to be multiplied.
                -> Int      -- ^ The exponent of the variable x^e to multiply the polynomial by.
                -> Poly     -- ^ The polynomial multiplied by the variable to the power of the integer.
multPolyVar xs i = replicate i 0 ++ xs

-- | 'gcdPoly' takes two polynomials and returns the greatest common divisor of them.
gcdPoly     :: Poly
            -> Poly
            -> Poly     -- ^ The greatest common divisor of the two polynomials.
gcdPoly [0] ys = ys
gcdPoly xs [0] = xs
gcdPoly xs ys 
    | greaterPoly xs ys     = gcdPoly ys (modDivPolyRemainder xs ys)
    | otherwise             = gcdPoly xs (modDivPolyRemainder ys xs)

-- | Calculates a solution for a polynomial with a given x.
solution    :: Poly     -- ^ The polynomial to be solved.
            -> Int      -- ^ The x to solve the polynomial for.
            -> Int      -- ^ The solution of the polynomial for the given x.
solution as x = sum [a * x^i | (a, i) <- zip as [(0::Integer)..]]