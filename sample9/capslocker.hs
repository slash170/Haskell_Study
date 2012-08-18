{-# OPTIONS -Wall #-}

import Control.Monad
import Data.Char

main :: IO a
main = forever $ do
         l <- getLine
         putStrLn $ map toUpper l