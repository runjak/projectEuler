module Main where

type N = Int

fac :: N -> N
fac 0 = 1
fac n = n * fac (n - 1)

facs = map fac [0..9]
