{-# LANGUAGE TypeApplications #-}
import Data.List ( sort )

main :: IO ()
main = do
  n <- read <$> getLine
  input <- getLine
  let s = sum $ map (read @Int) $ words input
  let s' = n * (n+1) `div` 2
  putStrLn $ show (s' - s)

