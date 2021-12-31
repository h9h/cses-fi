{-
  No. 1072

  Your task is to count for k=1,2,…,n the number of ways two knights can 
  be placed on a k×k chessboard so that they do not attack each other.

  [source](https://math.stackexchange.com/questions/3266257/number-of-ways-two-knights-can-be-placed-such-that-they-dont-attack)

  Note that when we have two knights threatening each other, it actually 
  forms either a 2×3 or 3×2 board. And for each of 2×3 and 3×2 boards, 
  there are 2 ways of placing two knights so that they threaten each other. 
  So, what we should do is to count how many 2×3 and 3×2 squares on n×n board. 
  For general n, the answer is

    (n−1)(n−2)+(n−2)(n−1) = 2(n−1)(n−2)

  And for each 2×3 and 3×2 board, there are 2 ways of placing the knights so 
  that they threaten each other. Therefore, in total there are

    4(n−1)(n−2)

  ways of placing two knights so that they threaten each other. So what we are 
  looking for is

    n^2(n^2−1)/2 − 4(n−1)(n−2)

  It is also worth mentioning that we are not over-counting because whenever 
  we place two knights so that they threaten each other, either a 2×3 or 3×2 
  board must contain both of the knights.

-}
import Control.Monad ( forM_ ) 

threats :: Int -> Int
threats n = n^2 * (n^2 - 1) `quot` 2 - 4 * (n - 1) * (n - 2)

main :: IO ()
main = readLn >>= (\k -> forM_ [1..k] (print . threats))
