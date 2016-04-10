module Main where

import Data.Char (digitToInt)
import Data.List

type N = Int

{-
  I'm encoding the squares as tuples of digits.
  From these I want to generate two sets,
  where each tuple implies, that if one of its digits is in one set,
  the other digit must be in the other set.
-}
squares :: [(N,N)]
squares = [(0,1),(0,4),(0,9),(1,6),(2,5),(3,6),(4,9),(6,4),(8,1)]
