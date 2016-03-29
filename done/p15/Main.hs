module Main where

import Data.List (genericReplicate)

gridSize :: Integer
gridSize = 20

startList :: [Integer]
startList = [1]

deduceList :: [Integer] -> [Integer]
deduceList [] = []
deduceList x = init $ expandList' x

expandList :: [Integer] -> [Integer]
expandList [] = []
expandList x@(a:_) = a : expandList' x

expandList' :: [Integer] -> [Integer]
expandList' (a:b:cs) = (a+b):expandList' (b:cs)
expandList' (a:as) = a : expandList as
expandList' [] = []

solution =
  let expansion = foldl (flip id) startList $ genericReplicate gridSize expandList
  in head . foldl (flip id) expansion $ genericReplicate gridSize deduceList

main = print solution
