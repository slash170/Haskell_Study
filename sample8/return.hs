{-# OPTIONS -Wall #-}

module Main where

main = do
  return ()
  a <- return "HAHAHA"
  line <- getLine
  return "BLAH BLAH BLAH"
  return 4
  putStrLn $ line ++ a