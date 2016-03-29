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
  | otherwise = all (/=0) . map (mod x) $ takeWhile (<=(root x)) primes

predicate :: N -> Bool
predicate p = let f = init . tail
                  s = show p
                  ps = map read $ f (tails s) ++ f (inits s)
              in all isPrime ps

searchSpace = dropWhile (< 10) primes

wanted = take 11 $ filter predicate searchSpace

solution = sum wanted

main = print solution
