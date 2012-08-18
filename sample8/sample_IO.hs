{-# OPTIONS -Wall #-}

module Main where

main = do
  putStr "Hey, "
  putStr "I'm "
  putStr "Andy!"
  putStrLn ""
  putChar 't'
  putChar 'e'
  putChar 'h'
  putStrLn ""
  print True
  print 2
  print "haha"
  print 3.2
  print [3,4,3]
  putStrLn ""
  rs <- sequence [getLine, getLine, getLine]
  print rs