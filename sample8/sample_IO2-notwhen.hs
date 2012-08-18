{-# OPTIONS -Wall #-}

module Main where

main = do
  input <- getLine
  if (input == "SWORDFISH")
    then putStrLn input
    else return()