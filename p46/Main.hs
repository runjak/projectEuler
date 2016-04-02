module Main where

import Control.Monad

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

composites :: [N]
composites = go [4..] primes
  where
    go xAll@(x:xs) pAll@(p:ps)
      | x < p  = x:go xs pAll
      | x == p = go xs ps
      | x > p  = go xAll ps

squares :: [N]
squares = [x*x | x <- [1..]]

wanted = do
  c <- composites
  p <- takeWhile (< c) primes
  s <- takeWhile (< (c - p)) squares
  guard $ c == 2*s + p -- These are the ones we DON'T want!
  return c

solution = head wanted

main = print solution
