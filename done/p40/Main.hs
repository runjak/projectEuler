module Main where

import Data.Char (digitToInt)

type N = Int

digits = concatMap show [0..]

d :: N -> N
d = digitToInt . (!!) digits 

solution = product $ map d [1, 10, 100, 1000, 10000, 100000, 1000000]

main = print solution
