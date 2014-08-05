module Main where

import Control.Monad
import Control.Monad.Writer (Sum)
import Data.List
import Data.Monoid

import qualified Control.Monad.Writer as Writer

digits = [0..9] :: [Int]

mkTriplets :: [Int] -> [(Int,Int,Int)]
mkTriplets (a:xs@(b:c:_)) = (a,b,c):mkTriplets xs
mkTriplets _ = []

triplets :: [[(Int, Int, Int)]]
triplets = map mkTriplets $ permutations digits

fromTriplets :: [(Int,Int,Int)] -> [Int]
fromTriplets ((a,b,c):xs)
  | xs == []  = a:b:[c]
  | otherwise = a:fromTriplets xs

toNumber :: [Int] -> Int
toNumber = foldl1 (\a b -> a*10+b)

chkNums = [2,3,5,7,11,13,17] :: [Int]

chk :: Int -> (Int,Int,Int) -> Bool
chk n (a,b,c) = (a*100+b*10+c) `rem` n == 0

chks = map chk chkNums

pass = and . zipWith ($) chks . tail

single :: [(Int,Int,Int)] -> Sum Int
single t
  | pass t    = Sum . toNumber $ fromTriplets t
  | otherwise = Sum 0

mySum = foldl1 (<>) $ map single triplets

main :: IO ()
main = print $ getSum mySum
