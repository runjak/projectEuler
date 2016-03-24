module Main where

import Control.Monad (liftM)
import Data.Array

type Point = (Int,Int)

file :: IO String
file = readFile "grid.txt"

cells :: String -> [Integer]
cells = map read . words

toArray :: [Integer] -> Array Point Integer
toArray = array ((1,1),(20,20)) . zip [(x,y)|x<-[1..20],y<-[1..20]]

field :: IO (Array Point Integer)
field = liftM (toArray . cells) file

multiValue :: Array Point Integer -> [Point] -> Integer
multiValue a = product . map ((!)a)

pointToRow :: Point -> [Point]
pointToRow (x,y) = take 4 [(x,z)|z<-[y..]]

pointToCol :: Point -> [Point]
pointToCol (x,y) = take 4 [(z,y)|z<-[x..]]

pointToDi1 :: Point -> [Point]
pointToDi1 (x,y) = take 4 [(x+z,y+z)|z<-[0..]]

pointToDi2 :: Point -> [Point]
pointToDi2 (x,y) = take 4 [(x+z,y-z)|z<-[0..]]

rows = map pointToRow [(x,y)|x<-[1..20],y<-[1..17]]
cols = map pointToCol [(x,y)|x<-[1..17],y<-[1..20]]
diags1 = map pointToDi1 [(x,y)|x<-[1..17],y<-[1..17]]
diags2 = map pointToDi2 [(x,y)|x<-[1..17],y<-[4..20]]
groups = rows++cols++diags1++diags2

main :: IO ()
main = do
  f <- field
  let m = maximum $ map (multiValue f) groups
  putStrLn $ show m
