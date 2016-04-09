module Main where

import Control.Monad
import Data.Function (on)
import Data.List
import Data.Set (Set)
import qualified Data.Maybe as Maybe
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

upperBound :: N
upperBound = upperBoundFor pSet 21

{-
  Function to calculate the upper bound
  for a set and a given window width
-}
upperBoundFor :: Set N -> N -> N
upperBoundFor set width =
  let primes = Set.toDescList set
      primeWindows = zip primes $ windowSums width primes
      uppers = map fst $ dropWhile ((> magic) . snd) primeWindows
  in Maybe.fromMaybe 0 $ Maybe.listToMaybe uppers

{-
  Set.size searchSpace == 4918
  Set.size pSet == 78498
-}
searchSpace :: Set N
searchSpace = fst $ Set.split (upperBound + 1) pSet

{-
  Produces a list of window widths and searchSpace sets.
  The list starts with width 21 and searchSpace as elements.
  Following elements are created via upperBoundFor
  by successively increasing width until the empty set is reached
  or width has reached the set size.
-}
windowSpacePairs :: [(N, Set N)]
windowSpacePairs = takeWhile predicate $ iterate go (21, searchSpace)
  where
    predicate :: (N, Set N) -> Bool
    predicate (w, set) = let nonEmpty = not $ Set.null set
                             wSmallerSize = w <= Set.size set
                         in nonEmpty && wSmallerSize

    go :: (N, Set N) -> (N, Set N)
    go (width', set) = let width = succ width'
                           uBound = upperBoundFor set width
                           noUpper = fst . Set.split (uBound + 1)
                       in (width, noUpper set)

rankedPrimes :: [(N, [N])]
rankedPrimes = do
  (width, set) <- windowSpacePairs
  let primes = filter isPrime $ windowSums width $ Set.toAscList set
  guard $ not $ null primes
  return (width, primes)

solution = head $ snd $ last rankedPrimes

main = print solution
