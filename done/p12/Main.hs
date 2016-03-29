module Main where

import Control.Monad (guard)

triangles :: [Integer]
triangles = helper 0 [1..]
  where
  helper n (x:xs) =
    let n' = n+x
    in n' : helper n' xs

someDivisors :: Integer -> [Integer]
someDivisors num = do
  let limit = floor . sqrt $ fromIntegral num
  x <- [2..limit]
  guard (num `mod` x == 0)
  return x

getDivisorCount :: Integer -> Int
getDivisorCount num = 2 * (length $ someDivisors num) + 2

solve :: Integer
solve = head $ filter (\x -> getDivisorCount x >= 500) triangles

main = putStrLn $ show solve
