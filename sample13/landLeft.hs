{-# OPTIONS -Wall #-}

import Control.Monad

type Birds = Int
type Pole = (Birds, Birds)


landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

routine :: Maybe Pole
routine = do
  start <- return (0, 0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

foo :: Maybe String
foo = Just 3  >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

foo' :: Maybe String
foo' = do
  x <- Just 3
  y <- Nothing
  Just (show x ++ y)

wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just ""
  return x

sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  guard ('7' `elem` show x)
  return x