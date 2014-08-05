module Main where

modulus = 10000000000 :: Integer

myPow :: Integer -> Integer -> Integer
myPow x 0 = 1
myPow x y = let p = myPow x (y - 1)
            in (x * p) `rem` modulus

pows = map (\x -> myPow x x) [1..1000]

mySum :: [Integer] -> Integer
mySum (x:xs) = (x + mySum xs) `rem` modulus
mySum [] = 0

main = print $ mySum pows
