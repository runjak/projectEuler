module Main where

import Control.Monad
import Data.Ratio ((%))
import qualified Data.Ratio as Ratio

type N = Int

ratios :: [(N, N)]
ratios = [(y,x)| x <- xs, y <- takeWhile (< x) xs]
  where
    xs = [x| x <- [11..99], x `mod` 10 /= 0]

same :: (N, N) -> (N, N) -> Bool
same (a,b) (c,d) = a%b == c%d

predicate :: (N, N) -> Bool
predicate ratio@(x, y) = or $ do
  let x' = show x
      y' = show y
  d <- show x
  guard $ d `elem` y'
  let y'' = go d y'
      x'' = go d x'
  guard $ same ratio (read x'', read y'')
  return True
  where
    go :: Char -> String -> String
    go c (x:xs)
      | c == x = xs
      | otherwise = x : go c xs
    go c [] = []

wanted = filter predicate ratios

solution = Ratio.denominator $ product $ map (uncurry (%)) wanted

main = print solution
