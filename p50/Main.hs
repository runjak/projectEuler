module Main where

import Control.Monad
import Data.Function (on)
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

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

{-
  I can reduce the searchSpace with the following knowledge:
     953 is prime and the sum of 21 consecutive primes
  -> I only need to consider a range, so that n >= 21 primes are <= 10^6.
  -> I can use this to find an upper limit for the set of primes I need to search.
  -> I also know that this must be within the first half of pSet,
     because n >= magic/2 implies that only two terms can be summed
     up to something bigger than magic.
-}

searchSpace' = fst $ Set.split (magic `div` 2) pSet

{- Calculate sequential sums of a list, starting with 0 -}
seqSums :: [N] -> [N]
seqSums = seqSums' 0
  where
    seqSums' s [] = [s]
    seqSums' s (n:ns) = s : seqSums' (s+n) ns

{- Calculate the sums of all segments of a given length in a given list. -}
windowSums :: N -> [N] -> [N]
windowSums width xs = let sums = seqSums xs
                      in zipWith subtract sums $ drop width sums
