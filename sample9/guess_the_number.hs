{-# OPTIONS -Wall #-}

import System.Random
import Control.Monad(when)

main :: IO ()
main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
  putStrLn "Which number in the range from 1 to 10 am I thinking of? "
  numberString <- getLine
  when (not $ null numberString) $ do
                     let readsList = reads numberString
                     outputAnswer readsList randNumber
                     askForNumber newGen

outputAnswer :: [(Int, String)] -> Int -> IO ()
outputAnswer [(number, _)] randNumber = do
  if randNumber == number
    then putStrLn "You are correct!"
    else putStrLn $ "Sorry, it was " ++ show randNumber
outputAnswer _ randNumber = do
  putStrLn $ "Sorry, it was " ++ show randNumber
