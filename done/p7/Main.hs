module Main where

primes :: [Integer]
primes = sieve [x|x<-[2..]]
	where
	sieve :: [Integer] -> [Integer]
	sieve (x:xs) = x : sieve [y|y<-xs, y`mod`x /= 0]

problem7 :: Integer
problem7 = last $ take 10001 primes

main = print problem7
