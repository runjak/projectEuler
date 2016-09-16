module Problem31 where
{--
  Task description:
  In England the currency is made up of pound, £, and pence, p,
  and there are eight coins in general circulation:

  1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
  It is possible to make £2 in the following way:

  1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
  How many different ways can £2 be made using any number of coins?
--}

import Control.Monad
import Data.List

type N = Int

coins :: [N]
coins = [200,100,50,20,10,5,2,1]

compositions :: N -> [[N]]
compositions = compositions' coins

compositions' :: [N] -> N -> [[N]]
compositions' _ 0 = return []
compositions' coins n = do
  let subs = filter (<=n) coins
  s <- subs -- we want one list for every choice here.
  let n'    = n - s
      subs' = filter (<=s) subs
  xs <- compositions' subs' n'
  return (s:xs)

main = print . length . compositions $ maximum coins
