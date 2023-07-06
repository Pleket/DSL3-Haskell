module Main where
import Algebra

main :: IO ()
-- main = putStrLn "Hello, Haskell!"
main = do
    -- pol = 1 + 2x + 3x^2 + 4x^3
    let pol = [1,-2,-3, 0, 9]
    -- 1 - 2x - 3x^2 + 9x^4
    print (showPoly pol)

    -- 1 + 2x + 3x^2 + 4x^3 * 3x^7
    print (showPoly 
        (addPoly 
            (subtractPoly pol [0, 0, 0, 0, 4])
            (multPoly [0, 0, 0, 0, 4] [0,0,0,0,0,0,0,3])
        ))
    -- OR
    print (showPoly (addPoly 
            [1, -2, -3]
            (multPoly [0, 0, 0, 0, 4] [0,0,0,0,0,0,0,3])
        ))
    
    -- (1 + 2x + 3x^2 + 4x^3) * 3x^7
    print (showPoly (multPoly pol [0,0,0,0,0,0,0,3]))