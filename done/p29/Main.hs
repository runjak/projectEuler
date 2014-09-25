module Main where

import Data.List

range = [2..100]

powers = [a^b|a<-range,b<-range]

main = print $ length $ nub powers
