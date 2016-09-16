module Problem42 where
{--
  Task description:
  The n^(th) term of the sequence of triangle numbers is given by, t_(n) = ½n(n+1); so the first ten triangle numbers are:

  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

  By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value.
  For example, the word value for SKY is 19 + 11 + 25 = 55 = t_(10). If the word value is a triangle number then we shall call the word a triangle word.

  Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?
--}

wordList :: IO [String]
wordList = do
  input <- readFile "p42_words.txt"
  let str = '[':input `mappend` "]"
  return (read str :: [String])

charScore :: Char -> Int
charScore = subtract 64 . fromEnum

score :: String -> Int
score = sum . fmap charScore

scores :: [String] -> [Int]
scores = fmap score

triangleNumbers :: [Int]
triangleNumbers = take 20 [(n*(n+1))`div`2 | n<-[1..]] -- I know that 20 is enougth :P

triangleScores :: [Int] -> [Int]
triangleScores = filter (`elem` triangleNumbers)

triangleWords :: [String] -> Int
triangleWords = length . triangleScores . scores

main :: IO ()
main = do
  wl <- wordList
  let foo = triangleWords wl
  print foo
