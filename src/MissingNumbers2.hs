import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.List (unfoldr)

{-
Much faster than first reading in all as String and the converting to Num
-} 
readInt :: B.ByteString -> Maybe (Int, B.ByteString)
readInt = B.readInt . B.dropWhile (not . isDigit)
 
main :: IO ()
main = do
    l <- getLine
    nums <- unfoldr readInt <$> B.getLine
    let n = read l
    let total = n * (n+1) `div` 2
    print (total - sum nums)