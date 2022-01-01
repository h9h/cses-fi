{-
  No. 1618

  Your task is to calculate the number of trailing zeros in the factorial n!.

  For example, 20! = 2432902008176640000 and it has 4 trailing zeros.
-}

trailingZeros :: Int -> Int
trailingZeros n 
  | n >= 5     = (n `quot` 5) + trailingZeros (n `quot` 5)
  | otherwise = 0

main :: IO ()
main = (print . trailingZeros) =<< readLn