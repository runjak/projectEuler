module Main where

-- Generating the fibonacci terms:
fibonacci :: Integer -> Integer
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci (n+1) = fibonacci n + fibonacci (n-1)

-- ->Found a pattern for the even ones:
evenFibonacci :: Integer -> Integer
evenFibonacci 0 = 2
evenFibonacci 1 = 8
evenFibonacci (n+1) = (evenFibonacci n) * 4 + evenFibonacci (n-1)

-- little helpy filter :P
evenList :: Integer -> [Integer]
evenList x = if (evenFibonacci x) <= 4000000
	then (evenFibonacci x : (evenList (x+1)))
	else []

problem2 :: () -> Integer
problem2 () = sum (evenList 0)

main = print problem2
