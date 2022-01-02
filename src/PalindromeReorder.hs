{-
  No. 1755

  Given a string, your task is to reorder its letters in such a way that 
  it becomes a palindrome (i.e., it reads the same forwards and backwards).
-}

import           Data.List           ( partition, foldl' )
import qualified Data.Map.Strict as Map
 
freq :: String -> [(Char, Int)]
freq = Map.toList . foldl' insert Map.empty 
  where insert haystack needle = Map.insertWith (+) needle 1 haystack

palindrome :: String -> String
palindrome s = palin
  where
        (odds, evens) = partition (odd . snd) $ freq s
        half = concatMap (\(c, l) -> replicate (l `quot` 2) c) evens
        palin
          | length odds > 1  = "NO SOLUTION"
          | length odds == 1 = reverse half ++ uncurry (flip replicate) (head odds) ++ half
          | otherwise        = reverse half ++ half

main :: IO ()
main = putStrLn . palindrome =<< getLine