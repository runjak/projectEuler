module Main where

import Data.Maybe as M

magic :: Integer
magic = 10^9
magicT = TS 1 magic
divisor :: Integer
divisor = 7
pows = tail $ iterate (*divisor) 1

data TriangleStump = TS{
      low :: Integer
    , up :: Integer
  }deriving (Show)

gSum :: Integer -> Integer
gSum x = (x^2+x)`div`2

size :: TriangleStump -> Integer
size ts = (gSum $ up ts) - (gSum . subtract 1 $ low ts)
