module Problem46 where
{--
  Task description:
  It was proposed by Christian Goldbach that every odd composite number
  can be written as the sum of a prime and twice a square.

  9 = 7 + 2×12
  15 = 7 + 2×22
  21 = 3 + 2×32
  25 = 7 + 2×32
  27 = 19 + 2×22
  33 = 31 + 2×12

  It turns out that the conjecture was false.

  What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
--}

import Control.Monad

type N = Integer

primes :: [N]
primes = 2 : 3 : sieve [] (tail primes) 3
  where
  notDivsBy d n     = n `rem` d /= 0
  sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2, x+4..p*p-2] ds
                   `mappend` sieve (p:ds) ps (p*p)

composites :: [N]
composites = go [4..] primes
  where
    go xAll@(x:xs) pAll@(p:ps)
      | x < p  = x:go xs pAll
      | x == p = go xs ps
      | x > p  = go xAll ps

squares :: [N]
squares = [x*x | x <- [1..]]

wanted = do
  c <- filter odd composites
  guard . notElem c $ do
    p <- takeWhile (< c) primes
    s <- takeWhile (< (c - p)) squares
    return $ 2*s+p
  return c

solution = head wanted

main = print solution
