{-
  No. 1622

  Given a string, your task is to generate all different strings that can 
  be created using its characters.

  Input

  The only input line has a string of length n. Each character is between a–z.

  Output

  First print an integer k: the number of strings. Then print k lines: the 
  strings in alphabetical order.
-}

import Data.List ( group, sort )

{- slow
permutate :: (Eq a, Ord a) => [a] -> [[a]]
permutate [] = [[]]
permutate l = [a:x | a <- l, x <- permutate $ delete a l]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)
-}

-- fast
inserts :: [a] -> [a] -> [[a]]
inserts [] ys = [ys]
inserts xs [] = [xs]
inserts xs@(x:xt) ys@(y:yt) = [x:zs | zs <- inserts xt ys] ++ [y:zs | zs <- inserts xs yt]

uniquePermutations :: Ord a => [a] -> [[a]]
uniquePermutations = foldr (concatMap . inserts) [[]] . group . sort

main:: IO ()
main = getLine >>= ((>>) <$> (print.length) <*> mapM_ putStrLn) . sort . uniquePermutations