{-
  No. 1092

  Your task is to divide the numbers 1,2,…,n into two sets of equal sum.
-}

createSet :: Bool -> [Int] -> Int -> [Int]
createSet first xs n
  | n <= 0    = xs
  | first     = n : createSet False xs (n-3)
  | otherwise = n : createSet True  xs (n-1)

construct :: Int -> IO ()
construct k = do
  putStrLn "YES"
  let p = createSet True  [] k
  let q = createSet False [] (k-1)
  print $ length p
  putStrLn $ unwords $ map show p
  print $ length q
  putStrLn $ unwords $ map show q


main :: IO ()
main = do
  k <- readLn
  let s = k * (k + 1) `quot` 2
  if even s then construct k else putStrLn "NO"
