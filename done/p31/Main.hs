module Main where

import Control.Monad
import Data.List

type N = Int

coins :: [N]
coins = [200,100,50,20,10,5,2,1]

compositions :: N -> [[N]]
compositions n = compositions' coins n

compositions' :: [N] -> N -> [[N]]
compositions' _ 0 = return []
compositions' coins n = do
  let subs = filter (<=n) coins
  s <- subs -- we want one list for every choice here.
  let n'    = n - s
      subs' = filter (<=s) subs
  xs <- compositions' subs' n'
  return (s:xs)

main = print $ length $ compositions $ maximum coins
