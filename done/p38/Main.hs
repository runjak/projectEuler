module Main where

import Data.List

type N = Int

digits :: String
digits = "123456789"

searchSpace :: [N]
searchSpace = [1..9999]

pandigital :: N -> Bool
pandigital = (== digits) . sort . show

cProds :: N -> [N]
cProds n = let ns = tail $ inits $ map (*n) [1..9]
           in map (read . concatMap show) ns

cProdSpace = concatMap cProds searchSpace

solution = maximum $ filter pandigital cProdSpace

main = print solution
