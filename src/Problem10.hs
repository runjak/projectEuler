module Problem10 where
{--
  Task description:
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.
--}

primes :: [Integer]
primes = 2: 3: sieve [] (tail primes) 3
  where
  notDivsBy d n     = n `mod` d /= 0
  sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2,x+4..p*p-2] ds
                    `mappend` sieve (p:ds) ps (p*p)

solve :: Integer
solve = sum $ takeWhile (<2000000) primes

main = print solve
