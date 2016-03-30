module Main where

import System.IO (readFile)
import Data.Char (isDigit)
import Data.List (nub)

input :: IO [(Char,Char,Char)]
input = do
  rawData <- readFile "keylog.txt"
  let triplets = mkTriplets $ filter isDigit rawData
  return $ nub triplets

mkTriplets :: String -> [(Char,Char,Char)]
mkTriplets (a:b:c:xs) = (a,b,c):mkTriplets xs
mkTriplets _ = []

setFirst :: Char -> Char -> String -> String
setFirst a b (x:xs)
  | x == a = x:xs
  | x == b = a:replace a b xs
  | otherwise = x : setFirst a b xs
  where
    replace :: Char -> Char -> String -> String
    replace a b (x:xs)
      | x == a = b : xs
      | otherwise = x : replace a b xs

imposeTriplet :: String -> (Char,Char,Char) -> String
imposeTriplet list (a,b,c) = setFirst a b . setFirst a c $ setFirst b c list

main :: IO ()
main = do
  triplets <- input
  let code = foldl imposeTriplet "01236789" triplets
  print code
