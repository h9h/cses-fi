{-
  No. 1069

  You are given a DNA sequence: a string consisting of characters A, C, G, and T. 
  Your task is to find the longest repetition in the sequence. This is a 
  maximum-length substring containing only one type of character.
-}

longestSequence :: String -> (Int, Char, Int)
longestSequence = foldl (\(maxn, c, n) c' -> if c' == c
                                             then (max maxn (n+1), c', n+1)
                                             else (maxn, c', 1))
                  (1, '-', 0)

main :: IO ()
main = do
  input <- getLine
  let (n, _, _) = longestSequence input
  print n

{-
import qualified Data.ByteString.Char8 as B
 
main = B.getLine >>= print . maximum . map B.length . B.group
-}