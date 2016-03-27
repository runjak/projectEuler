module Main where

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
                   ++ sieve (p:ds) ps (p*p)

root :: N -> N
root = round . sqrt . fromIntegral

isPrime :: N -> Bool
isPrime x
  | x <= 1 = False
  | otherwise = all (/=0) . map (mod x) $ takeWhile (<=(root x)) primes

startSet :: Set N
startSet = Set.fromAscList $ takeWhile (<= (magic `div` 2)) primes

project :: N -> Set N -> Set N
project p = let upperBound = fst . Set.split ((magic `div` p) + 1)
                lowerBound = snd . Set.split (subtract 1 p)
            in lowerBound . upperBound

solution :: N
solution = sum $ do
  p1 <- Set.toAscList $ startSet
  return $ Set.size $ project p1 startSet

main :: IO ()
main = print solution
