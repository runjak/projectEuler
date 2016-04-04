module Main where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type N = Integer

primes :: [N]
primes = 2 : 3 : sieve [] (tail primes) 3
  where
  notDivsBy d n     = n `rem` d /= 0
  sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2, x+4..p*p-2] ds
                   ++ sieve (p:ds) ps (p*p)

magic = 10^6 :: N

pSet :: Set N
pSet = Set.fromAscList $ takeWhile (<= magic) primes

isPrime :: N -> Bool
isPrime = Set.member `flip` pSet

-- | This approach filled all my ram and took too long.
--   I should refine it.

sums :: [[N]]
sums = go ps $ tail ps
  where
    ps = Set.toAscList pSet

    go _ [] = []
    go [] _ = []
    go xs ys = let newRow = zipWith (+) xs ys
               in newRow : go newRow (tail ys)

wanted :: [N]
wanted = filter isPrime $ concat $ reverse sums

solution :: N
solution = head wanted

main = print solution
