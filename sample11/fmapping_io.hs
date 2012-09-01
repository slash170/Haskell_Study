{-# OPTIONS -Wall #-}

module Main (main) where

import Data.Char
import Data.List

main :: IO ()
main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line
