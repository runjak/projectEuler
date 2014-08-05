module Main where

solution :: Integer
solution = read . reverse . take 10 . reverse . show $ sum [x^x|x<-[1..1000]]

main = print solution
