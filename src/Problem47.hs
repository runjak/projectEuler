module Problem47 where
{--
  Task description:
  The first two consecutive numbers to have two distinct prime factors are:

  14 = 2 × 7
  15 = 3 × 5

  The first three consecutive numbers to have three distinct prime factors are:

  644 = 2² × 7 × 23
  645 = 3 × 5 × 43
  646 = 2 × 17 × 19.

  Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?
--}

import Control.Monad
import Data.List

type N = Integer

primes :: [N]
primes = 2 : 3 : sieve [] (tail primes) 3
  where
  notDivsBy d n     = n `rem` d /= 0
  sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2, x+4..p*p-2] ds
                   `mappend` sieve (p:ds) ps (p*p)

root :: N -> N
root = round . sqrt . fromIntegral

isPrime :: N -> Bool
isPrime x
  | x <= 1 = False
  | otherwise = notElem 0 . fmap (mod x) $ takeWhile (<= root x) primes

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

withScore = zip searchSpace $ fmap (length . nub . explode) searchSpace

wanted = fst <$> filter ((4==) . snd) withScore

solve :: [N] -> N
solve (a:b:c:d:xs)
  | [b,c,d] == fmap (+1) [a,b,c] = a
  | otherwise = solve $ b:c:d:xs

solution = solve wanted

main = print solution
