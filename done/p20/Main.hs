module Main where

import Data.Char (digitToInt)

fac :: Integer -> Integer
fac 1 = 1
fac x = fac (x-1) * x

magicNumber :: Integer
magicNumber = fac 100

digitSum :: Int
digitSum = sum . map digitToInt $ show magicNumber

main = print digitSum
