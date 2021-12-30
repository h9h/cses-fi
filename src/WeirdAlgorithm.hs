{-
  No. 1068

  Consider an algorithm that takes as input a positive integer n. If n is even, 
  the algorithm divides it by two, and if n is odd, the algorithm multiplies it 
  by three and adds one. The algorithm repeats this, until n is one. For example, 
  the sequence for n=3 is as follows:

    3→10→5→16→8→4→2→1

  Your task is to simulate the execution of the algorithm for a given value of n.
-}

-- | Calculates next number in sequence
weird :: Int -- ^the starting value 
      -> Int -- ^the resulting sequence
weird n
  | n == 1    = 1
  | even n    = n `div` 2  
  | otherwise = 3 * n + 1

collectWeird :: [Int] -> Int -> [Int]
collectWeird xs n
  | n == 1    = reverse (1 :xs)
  | otherwise = collectWeird (n:xs) (weird n)

main :: IO ()
main = do
  input <- getLine 
  let xs = collectWeird [] $ read input
  putStrLn $ concatMap ((++ " ") . show) xs
