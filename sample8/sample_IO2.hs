{-# OPTIONS -Wall #-}

module Main where

import Control.Monad

main = do
  input <- getLine
  when (input == "SWORDFISH") $ do
    putStrLn input