module Main where

fib :: [(Integer,Integer)]
fib = zip [1..] $ produce 1 1
  where
    produce :: Integer -> Integer -> [Integer]
    produce x y = x : produce y (x+y)

position :: Integer
position = fst $ head [x|x<-fib,(length . show $ snd x) >= 1000]

main = print position
