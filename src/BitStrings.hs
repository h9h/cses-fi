{-
  No. 1617

  Your task is to calculate the number of bit strings of length n.
  For example, if n=3, the correct answer is 8, because the possible bit 
  strings are 000, 001, 010, 011, 100, 101, 110, and 111.

  Print the result modulo 10^9+7.

  Note: 10^9+7 is the first prime number bigger than 1 billion. This
  is small enough to insure, that the result doesn't cause overflow. 
  Primeness ensures, that the results are spread out.
-}

main :: IO ()
main = (print . (`mod` 1000000007) . (2^)) =<< readLn