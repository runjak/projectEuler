module Main where

ring :: Int -> [Int]
ring 1 = [1]
ring x = take 4 $ iterate (subtract (x - 1)) (x^2)

rings :: [Int]
rings = concatMap ring [1,3..1001]

main = print $ sum rings
