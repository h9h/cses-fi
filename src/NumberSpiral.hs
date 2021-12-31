{-
  No. 1071

  A number spiral is an infinite grid whose upper-left square has number 1. 
  Here are the first five layers of the spiral:

           1   2   9  10  25
           4   3   8  11  24
           5   6   7  12  23
          16  15  14  13  22
          17  18  19  20  21

  Your task is to find out the number in row y and column x.

  Note:
  - The square numbers sit on:
    -  1 - (1,1)
    -  4 - (2,1)
    -  9 - (1,3)
    - 16 - (4,1)
    - 25 - (1,5)
-}

import Control.Monad ( replicateM_ )

getNumber :: [Int] -> Int
getNumber [y,x] = num y x
  where num r c
         | even r && c <= r = r * r - c + 1
         | odd c  && r <= c = c * c - r + 1
         | odd r  && c < r  = (r-1) * (r-1) + c
         | even c && r < c  = (c-1) * (c-1) + r
         | otherwise        = error "Bad input 1"
getNumber _ = error "Bad input 2"

main :: IO ()
main = do
  flip replicateM_ (print . getNumber . map read . words =<< getLine) =<< readLn