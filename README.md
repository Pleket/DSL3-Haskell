# DSL3-Haskell

The Haskell environment is designed in such a way that all implemented functions for polynomial arithmetic and number theory can be used in Main.hs. 

# Polynomial expressions
Algebra.hs, located in the src directory, hosts a self-created interpretation of polynomial arithmetic with integers. 
All exported functions are listed in the module Algebra brackets, and documentation of each of the functions is included. 
Algebra.hs supports polynomials as a list of integers and supports printing polynomials, addition, subtraction, multiplication, 
exponentiation and modular division. Through helper functions, multiplication with a polynomial x^i using value i is included, 
as well as separate functions for remainder and divisor. Lastly, solution was implemented to calculate a solution of the polynomial. 

# Primes
Primes.hs hosts the basis for primes in the Haskell environment. 
Using the arithmoi library, functions for all kinds of prime computations are imported with great performance. 
Primes.hs contains functions like isValidPrime and getNextPrime to change the library behaviour to desired behaviour. 
For example, isPrime does not return a boolean but a Maybe value, which is not very interpretable for the user. 
Additionally, nextPrime does not return the next prime when the input is a prime number, for which getNextPrime exists. 
Just like in Algebra.hs, documentation is included for every function. 

# Moduli, Coprimes
The arithmoi library also supported moduli and coprimes. These were imported in Coprimes.hs and Moduli.hs for use, 
but eventually not included in the final language. However, this can be a next step in the improvement of this DSL. 