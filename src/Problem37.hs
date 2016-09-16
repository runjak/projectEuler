module Problem37 where
{--
  Task description:
  The number 3797 has an interesting property.
  Being prime itself, it is possible to continuously remove digits from left to right,
  and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

  Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

  NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
--}

import Control.Monad
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

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

predicate :: N -> Bool
predicate p = let f = init . tail
                  s = show p
                  ps = fmap read $ f (tails s) `mappend` f (inits s)
              in all isPrime ps

searchSpace = dropWhile (< 10) primes

wanted = take 11 $ filter predicate searchSpace

solution = sum wanted

main = print solution
