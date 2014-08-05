module Main where

import Control.Arrow
import Data.Char (digitToInt)
import Data.List (sort, partition)

{- The given chain function -}
next :: Int -> Int
next = sum . map ((^2) . digitToInt) . show

{- | This will also kill the zeroes :) -}
norm :: Int -> Int
norm = read . sort . show

step :: Int -> Int
step = next . norm

range :: [Int]
range = [1..10000000]

good :: Int -> Bool
good 89 = True
good _ = False

run :: [Int] -> (Int, [Int])
run = first length . partition good . filter (/= 1) . map step
