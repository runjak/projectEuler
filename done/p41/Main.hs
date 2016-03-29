module Main where

import Data.List

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

digits = "123456789"

pandigital :: N -> Bool
pandigital = (== digits) . sort . show

searchSpace :: [N]
searchSpace = do
  ds <- reverse $ tail $ inits digits
  map read $ permutations ds

solution = maximum $ filter isPrime searchSpace

main = print solution
