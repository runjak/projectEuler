module Main where

import Control.Monad (guard)
import Data.List (nub, zip3)

range :: [Int]
range = [1..9999]

intSqr :: Int -> Int
intSqr = floor . sqrt . fromIntegral

divisors :: Int -> [Int]
divisors x = (1:) . nub . concat $ do
  a <- [2..intSqr x]
  guard (x`rem`a == 0)
  return [a, x`div`a]

divSum :: Int -> Int
divSum = sum . divisors

amicables = map (\(x,_,_)->x) . filter (\(a,b,c)->a==c && a/=b) . (\(x:y:z:_)->zip3 x y z) . take 3 . iterate (map divSum)

main = print . sum $ amicables range
