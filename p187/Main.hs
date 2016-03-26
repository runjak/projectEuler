module Main where

import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

magic :: N
magic = 10^8

range :: [N]
range = [4..magic] -- | 4 is the first semiprime

primes :: [N]
primes = 2 : 3 : sieve [] (tail primes) 3
  where
  notDivsBy d n     = n `rem` d /= 0
  sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2, x+4..p*p-2] ds
                   ++ sieve (p:ds) ps (p*p)

root :: N -> N
root = round . sqrt . fromIntegral

isPrime :: N -> Bool
isPrime x
  | x <= 1 = False
  | otherwise = all (/=0) . map (mod x) $ takeWhile (<=(root x)) primes

subSet :: Set N -> N -> Set N
subSet s p = fst $ Set.split (magic `div` p) s

foo = let startSet = Set.fromAscList $ takeWhile (<= (magic `div` 2)) primes
          ps = takeWhile (<= (root magic) primes
      in go ps startSet
      where
        go :: [N] -> Set N -> [Set N]
        go (p:ps) s = s : go ps $ subS
        go [] s = [s]
