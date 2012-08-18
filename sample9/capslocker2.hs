{-# OPTIONS -Wall #-}

import Data.Char

main :: IO()
main = do
  contents <- getContents
  putStrLn $ map toUpper contents