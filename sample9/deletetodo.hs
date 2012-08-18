{-# OPTIONS -Wall #-}

import System.IO
import System.Directory
import Data.List

main :: IO ()
main = do
  contents <- readFile "todo.txt"
  let todoTasks = lines contents
      numberTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStrLn "These are your TO-DO items:"
  mapM_ putStrLn numberTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle newTodoItems
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"