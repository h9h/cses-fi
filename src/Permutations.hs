{-
  No. 1070

  A permutation of integers 1,2,…,n is called beautiful if there are no 
  adjacent elements whose difference is 1.

  Given n, construct a beautiful permutation if such a permutation exists.
  
  min n = 4: 2 4 1 3
-}

permutations :: Int -> String 
permutations n
  | n == 1    = "1"
  | n < 4     = "NO SOLUTION"
  | otherwise = unwords $ map show ([2,4..n] ++ [1,3..n])

main :: IO ()
main = do
    n <- read <$> getLine
    putStrLn $ permutations n