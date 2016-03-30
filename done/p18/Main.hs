module Main where

import System.IO (readFile)
import Control.Monad (liftM)

file :: IO String
--file = readFile "numbers.txt"
file = readFile "triangle.txt"

triangle :: String -> [[Integer]]
triangle = reverse . map (map read . words) . lines

mergeLines :: [Integer] -> [Integer] -> [Integer]
mergeLines (a:a':as) (b:bs)
  | a > a' = (a+b) : mergeLines (a':as) bs
  | otherwise = (a'+b) : mergeLines (a':as) bs
mergeLines _ _ = []

solution :: IO [Integer]
solution = do
  t <- liftM triangle file
  return $ foldl1 mergeLines t

main = print =<< solution
