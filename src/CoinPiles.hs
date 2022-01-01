{-
  No. 1754

  You have two coin piles containing a and b coins. On each move, 
  you can either remove one coin from the left pile and two coins 
  from the right pile, or two coins from the left pile and one coin 
  from the right pile.
-}

import Control.Monad ( replicateM_ )

checkPiles :: [Int] -> Bool
checkPiles [x,y] =  abs (x - y) <= min x y
                 && (x + y) `mod` 3 == 0             
checkPiles _ = error "Bad input 2"

main :: IO ()
main = do
  flip replicateM_ (putStrLn . (\x -> if x then "YES" else "NO"). checkPiles . map read . words =<< getLine) =<< readLn