module Main where

import Control.Monad (guard)

triplets :: [(Integer,Integer,Integer)]
triplets = do
	z <- [1..]
	x <- [1..z]
	y <- [x..z]
	guard (x^2 + y^2 == z^2)
	return (x, y, z)

correct :: (Integer, Integer, Integer) -> Bool
correct (x,y,z) = x+y+z == 1000

merge :: (Integer,Integer,Integer) -> Integer
merge (x,y,z) = x*y*z

solve = merge . head $ filter correct triplets

main = print solve
