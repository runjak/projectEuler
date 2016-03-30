module Main where

wordList :: IO [String]
wordList = do
  input <- readFile "words.txt"
  let str = '[':input++"]"
  return $ (read str :: [String])

charScore :: Char -> Int
charScore = subtract 64 . fromEnum

score :: String -> Int
score = sum . map charScore

scores :: [String] -> [Int]
scores = map score

triangleNumbers :: [Int]
triangleNumbers = take 20 [(n*(n+1))`div`2|n<-[1..]] -- I know that 20 is enougth :P

triangleScores :: [Int] -> [Int]
triangleScores = filter (`elem` triangleNumbers)

triangleWords :: [String] -> Int
triangleWords = length . triangleScores . scores

main :: IO ()
main = do
  wl <- wordList
  let foo = triangleWords wl
  putStrLn $ show foo
  return ()
