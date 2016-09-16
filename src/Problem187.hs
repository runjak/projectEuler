module Problem187 where
{--
  Task description:
  A composite is a number containing at least two prime factors. For example, 15 = 3 × 5; 9 = 3 × 3; 12 = 2 × 2 × 3.

  There are ten composites below thirty containing precisely two, not necessarily distinct, prime factors: 4, 6, 9, 10, 14, 15, 21, 22, 25, 26.

  How many composite integers, n < 10^8, have precisely two, not necessarily distinct, prime factors?
--}

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

magic :: N
magic = 10^8

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

startSet :: Set N
startSet = Set.fromAscList $ takeWhile (<= (magic `div` 2)) primes

project :: N -> Set N -> Set N
project p = let upperBound = fst . Set.split ((magic `div` p) + 1)
                lowerBound = snd . Set.split (subtract 1 p)
            in lowerBound . upperBound

solution :: N
solution = sum $ do
  p1 <- Set.toAscList startSet
  return . Set.size $ project p1 startSet

main :: IO ()
main = print solution
