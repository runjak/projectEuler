module Main where

import Data.Function (on)
import Data.List (maximumBy)

border :: Int
border = 999999

range = [1..border]

collatz :: (Integral a) => a -> a
collatz n
  | odd n = 3*n+1
  | otherwise = n`div`2

chain :: Int -> [Int]
chain 1 = [1]
chain x = let y = collatz x in y : chain y

score = length . chain

vals = [(score x, x)|x<-range]

main = print $ maximumBy (compare `on` fst) vals
