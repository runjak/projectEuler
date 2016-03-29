module Main where

import System.IO (readFile)
import Control.Monad (liftM)
import Data.List (sort)

file = readFile "names.txt"

mkLines :: String -> String
mkLines (x:xs)
  | x == '\"' = mkLines xs
  | x == ',' = '\n':mkLines xs
  | otherwise = x : mkLines xs
mkLines [] = []

sortedLines :: String -> [String]
sortedLines = sort . lines . mkLines

alphaScore :: String -> Integer
alphaScore = sum . map (flip (-) 64 . toInteger . fromEnum)

scores :: String -> [Integer]
scores = zipWith (*) [1..] . map alphaScore . sortedLines

Main :: IO ()
Main = print $ liftM (sum . scores) file
