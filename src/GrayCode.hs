{-
  No. 2205

  A Gray code is a list of all 2^n bit strings of length n, where any two successive 
  strings differ in exactly one bit (i.e., their Hamming distance is one).

  Your task is to create a Gray code for a given length n.

  Print 2^n lines that describe the Gray code. You can print any valid solution.
-}

import Data.Bits ( Bits(xor, shift) )
import Text.Printf ( printf )

grayCode :: Int -> [Int]
grayCode n = map (\i -> xor (shift i (-1)) i) [0..2^n-1]

{- Alternative: here we get the right length by construction
grayCode :: Int -> [String]
grayCode 0 = [""]
grayCode n =
  let prev = grayCode (n-1) in
   map ('0' :) prev ++
   map ('1' :) (reverse prev)
-}

main :: IO ()
main = do
  n <- readLn
  mapM_ (printf "%.*b\n" n) $ grayCode n
  