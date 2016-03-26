module Main where

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

magic :: N
magic = 10^8

range :: [N] -- FIXME check where this is useful
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

{-|
  Problems with solution encompass:
  1. It takes too long
  -> But I can make lists into sets and use faster projections for sets!
|-}
solution :: N
solution = length $ do
  let ps = takeWhile (<= (magic `div` 2)) primes
  p1 <- ps
  p2 <- takeWhile (<= (magic `div` p1)) $ dropWhile (< p1) ps
  let n = p1*p2
  guard $ n < magic
  return n

main :: IO ()
main = print solution
