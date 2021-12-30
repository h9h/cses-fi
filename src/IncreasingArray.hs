{-
  No. 1094

  You are given an array of n integers. You want to modify the array so that 
  it is increasing, i.e., every element is at least as large as the previous 
  element.

  On each move, you may increase the value of any element by one. What is the 
  minimum number of moves required?
-}

import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.List (unfoldr)

readInt :: B.ByteString -> Maybe (Int, B.ByteString)
readInt = B.readInt . B.dropWhile (not . isDigit)

increase :: [Int] -> Int -> Int -> Int
increase [] _ s = s
increase (x:xs) p s
  | x < p    = increase xs p (s + p - x)
  | otherwise = increase xs x s

main :: IO ()
main = do
    _ <- getLine
    xs <- unfoldr readInt <$> B.getLine
    print $ increase xs (head xs) 0