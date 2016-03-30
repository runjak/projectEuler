module Main where

problem6 :: Integer
problem6 = ((^2) $ sum range) - sum (map (^2) range)
  where
  range = [1..100]

main = print problem6
