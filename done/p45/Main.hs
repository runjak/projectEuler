module Main where

import Data.List.Ordered (mergeAll)

triangles = [n*(n+1)`div`2|n<-[1..]]
pentagonals = [n*(3*n-1)`div`2|n<-[1..]]
hexagonals = [n*(2*n-1)|n<-[1..]]

bag = mergeAll [triangles, pentagonals, hexagonals]

search :: [Integer] -> Integer
search (x:y:z:rest)
	| x == y && y == z = y
	| otherwise = search (y:z:rest)

solution = search $ dropWhile (<=40755) bag

main = print solution
