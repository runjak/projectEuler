module Main where

type Offset = Int
data Step = Small | Big deriving (Show)

step :: Offset -> Step -> Offset
step o Big = o + 1 `mod` 2
step
