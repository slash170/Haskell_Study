{-# OPTIONS -Wall #-}

import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch "bump" = bump
dispatch command = doesntExist command

doesntExist :: String -> [String] -> IO ()
doesntExist command _ = putStrLn $ "The " ++ command ++ " command doesn't exist"

main :: IO ()
main = do
  args <- getArgs
  case args of (command:argList) -> do
                 dispatch command argList
               _ -> do
                 putStrLn "usage:./todo.hs [command] [file]"

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "The add command takes exactly two arguments"

view :: [String] -> IO ()
view [fileName] = do
  fileExist <- doesFileExist fileName
  if fileExist
     then do
       contents <- readFile fileName
       let todoTasks = lines contents
           numberedTasks = zipWith (\n line -> show (n :: Integer) ++ " - " ++ line) [0..] todoTasks
       putStr $ unlines numberedTasks
     else do
       putStrLn $ fileName ++ " doesn't exist!!"
view _ = putStrLn "The view command takes exactly one arguments"

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  fileExist <- doesFileExist fileName
  if fileExist
     then do
       contents <- readFile fileName
       let todoTasks = lines contents
           number = read numberString
           newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
       outputTodo (fileName, newTodoItems)
     else do
       putStrLn $ fileName ++ " doesn't exist!!"
remove _ = putStrLn "The remove command takes exactly two arguments"

bump :: [String] -> IO ()
bump [fileName, numberString] = do
  fileExist <- doesFileExist fileName
  if fileExist
     then do
       contents <- readFile fileName
       let todoTasks = lines contents
           number = read numberString
           newTodoItems = unlines $ task:(delete task todoTasks)
               where task = (todoTasks !! number)
       outputTodo (fileName, newTodoItems)
     else do
       putStrLn $ fileName ++ " doesn't exist!!"
bump _ = putStrLn "The bump command takes exactly two arguments"

outputTodo :: (String, String) -> IO ()
outputTodo (fileName, newTodoItems) = do
  bracketOnError (openTempFile "." "temp")
       (\(tempName, tempHandle) -> do
          hClose tempHandle
          removeFile tempName)

       (\(tempName, tempHandle) -> do
          hPutStr tempHandle newTodoItems
          hClose tempHandle
          removeFile fileName
          renameFile tempName fileName)
