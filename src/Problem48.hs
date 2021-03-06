module Problem48 where
{--
  Task description:
  The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.

  Find the last ten digits of the series, 1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).
--}

solution :: Integer
solution = read . reverse . take 10 . reverse . show $ sum [x^x | x<-[1..1000]]

main = print solution
