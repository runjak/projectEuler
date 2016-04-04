module Main where

import Control.Monad
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
  | otherwise = notElem 0 . map (mod x) $ takeWhile (<= root x) primes

explode :: N -> [N]
explode n 
  | isPrime n = [n]
  | otherwise = do
    p <- takeWhile (<= root n) primes
    let (d, m) = divMod n p
    guard $ m == 0
    p:explode d

searchSpace :: [N]
searchSpace = [16..]

withScore = zip searchSpace $ map (length . nub . explode) searchSpace

wanted = map fst $ filter ((4==) . snd) withScore

solve :: [N] -> N
solve (a:b:c:d:xs)
  | [b,c,d] == map (+1) [a,b,c] = a
  | otherwise = solve $ b:c:d:xs

solution = solve wanted

main = print solution
