module Main where

import Data.List (sort)

sameDigits x y  = (==)(sort $ show x) (sort $ show y)

allSame :: [Int] -> Bool
allSame (x:xs) = all (sameDigits x) xs
allSame _ = False

vals = [map (x*) [2..6] | x <- [1..]]

results = flip zip [1..] $ map allSame vals

solution = snd . head $ filter fst results

main = print solution
