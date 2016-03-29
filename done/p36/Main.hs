module Main where

magic :: Int
magic = 1000000

range :: [Int]
range = [1..magic]

palindrom :: String -> Bool
palindrom s = s == reverse s

bTenPals :: [Int]
bTenPals = filter (palindrom . show) range

toBinStr :: Int -> String
toBinStr 0 = "0"
toBinStr 1 = "1"
toBinStr x
  | odd x = '1' : toBinStr ((x-1)`div`2)
  | otherwise = '0' : toBinStr (x`div`2)

pals :: [Int]
pals = filter (palindrom . toBinStr) bTenPals

main :: IO ()
main = print $ sum pals
