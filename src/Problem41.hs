module Problem41 where
{--
  Task description:
  We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.

  For example, 2143 is a 4-digit pandigital and is also prime.

  What is the largest n-digit pandigital prime that exists?
--}

import Data.List

type N = Integer

primes :: [N]
primes = 2 : 3 : sieve [] (tail primes) 3
  where
  notDivsBy d n     = n `rem` d /= 0
  sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2, x+4..p*p-2] ds
                   `mappend` sieve (p:ds) ps (p*p)

root :: N -> N
root = round . sqrt . fromIntegral

isPrime :: N -> Bool
isPrime x
  | x <= 1 = False
  | otherwise = notElem 0 . fmap (mod x) $ takeWhile (<= root x) primes

digits = "123456789"

pandigital :: N -> Bool
pandigital = (== digits) . sort . show

searchSpace :: [N]
searchSpace = do
  ds <- reverse . tail $ inits digits
  read <$> permutations ds

solution = maximum $ filter isPrime searchSpace

main = print solution
