module Problem7 where
{--
Task description:
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6^(th) prime is 13.

What is the 10001^(st) prime number?
--}

primes :: [Integer]
primes = sieve [2..]
  where
  sieve :: [Integer] -> [Integer]
  sieve (x:xs) = x : sieve [y | y<-xs, y`mod`x /= 0]

problem7 :: Integer
problem7 = last $ take 10001 primes

main = print problem7
