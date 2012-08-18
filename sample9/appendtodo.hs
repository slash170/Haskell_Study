{-# OPTIONS -Wall #-}

import System.IO

main :: IO ()
main = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")