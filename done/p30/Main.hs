module Main where

import Data.Char (digitToInt)

range = [2..]

f :: Int -> Int
f = sum . map ((^5) . digitToInt) . show

nums = filter (\x -> x == f x) range

main = print . sum $ take 6 nums
