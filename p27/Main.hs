module Main where

range :: [Int]
range = [-1000,-999..1000]

type A = Int
type B = Int
newtype QForm = QForm (A,B)

instance Show QForm where
  show (QForm (a, b)) = "n²+"++(show a)++"n+"++(show b)

calc :: QForm -> Int -> Int
calc (QForm (a,b)) n = n^2 + a*n + b

isPrime :: Int -> Bool
isPrime x
  | x < 2     = False
  | otherwise = x`elem`(takeWhile (<= x) primes)

primes :: [Int]
primes = sieve 2 [3,5..]
  where
    sieve :: Int -> [Int] -> [Int]
    sieve p (x:xs) = p : sieve x [y| y <- xs, y`mod`x /= 0]

