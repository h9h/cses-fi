{-
  No. 1069

  You are given a DNA sequence: a string consisting of characters A, C, G, and T. 
  Your task is to find the longest repetition in the sequence. This is a 
  maximum-length substring containing only one type of character.
-}

longest ::  Char -> Int -> Int -> [Char] -> Int
longest   _ _ maxi [] = maxi
longest current count maxi (x:xs) 
  | x == current = let maxi' = max (count + 1) maxi
                   in longest  current (count + 1) maxi' xs
  | otherwise    = let maxi' = max count maxi
                   in longest  x 1 maxi' xs
 
main :: IO ()
main = interact $ show . longest '_' 1 1