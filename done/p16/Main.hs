module Main where

import Data.Char (digitToInt)

magicNumber :: Integer
magicNumber = 2^1000

solution :: Int
solution = sum . map digitToInt $ show magicNumber

main :: IO ()
main = print solution
