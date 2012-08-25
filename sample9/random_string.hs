{-# OPTIONS -Wall #-}

import System.Random

main :: IO ()
main = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)
  gen2 <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen2)