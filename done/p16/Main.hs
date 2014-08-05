module Main where

import Data.Char (digitToInt)

magicNumber :: Integer
magicNumber = 2^1000

solution :: Integer
solution = sum . map digitToInt $ show magicNumber

main :: IO ()
main = putStrLn $ "solution:\t" ++ (show solution)
