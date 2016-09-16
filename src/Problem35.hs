module Problem35 where
{--
  Task description:
  The number, 197, is called a circular prime because all rotations of the digits:
  197, 971, and 719, are themselves prime.

  There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

  How many circular primes are there below one million?
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

magic :: N
magic = 1000000

candidates :: [N]
candidates = takeWhile (<= magic) primes

rotations :: N -> [N]
rotations n = let s = show n
                  zs = zipWith (++) (tails s) (inits s)
              in fmap read . init . tail $ zs

circularPrimes :: [[N]]
circularPrimes = do
  p <- candidates
  let rs = rotations p
  guard . all isPrime $ rotations p
  return $ p:rs

cPrimeSet = Set.fromList $ concat circularPrimes

solution = Set.size cPrimeSet

main = print solution
