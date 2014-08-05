module Main where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Set(Set)
import qualified Data.Set as S

type Divisor = Int
range :: [Divisor]
range = [2..999]

digits :: Int -> Divisor -> [(Int,Divisor)]
digits 0 _ = []
digits x y
  | x < y = digits (x * 10) y
  | otherwise = (x,y):digits (x`mod`y) y

findCircle :: Set (Int,Divisor) -> [(Int,Divisor)] -> Int
findCircle _ [] = 0
findCircle s (x:xs)
  | S.member x s = (+1) . length . fst $ break (==x) xs
  | otherwise = findCircle (S.insert x s) xs

compares :: [(Divisor,Int)]
compares = map (\r -> (r, findCircle S.empty $ digits 1 r)) range

target = maximumBy (compare `on` snd) compares

main = print $ fst target
