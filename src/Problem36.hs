module Problem36 where
{--
  Task description:
  The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

  Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

  (Please note that the palindromic number, in either base, may not include leading zeros.)
--}

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
