{-# OPTIONS -Wall #-}

import System.IO

main = do
  contents <- readFile "baabaa.txt"
  putStr contents