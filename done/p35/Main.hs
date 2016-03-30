module Main where

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
                   ++ sieve (p:ds) ps (p*p)

root :: N -> N
root = round . sqrt . fromIntegral

isPrime :: N -> Bool
isPrime x
  | x <= 1 = False
  | otherwise = notElem 0 . map (mod x) $ takeWhile (<= root x) primes

magic :: N
magic = 1000000

candidates :: [N]
candidates = takeWhile (<= magic) primes

rotations :: N -> [N]
rotations n = let s = show n
                  zs = zipWith (++) (tails s) (inits s)
              in map read $ init . tail $ zs

circularPrimes :: [[N]]
circularPrimes = do
  p <- candidates
  let rs = rotations p
  guard $ all isPrime $ rotations p
  return $ p:rs

cPrimeSet = Set.fromList $ concat circularPrimes

solution = Set.size cPrimeSet

main = print solution
