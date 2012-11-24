{-# OPTIONS -Wall #-}

import Control.Monad.Error()

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Right (left + n, right)
    | otherwise                    = banana (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Right (left, right + n)
    | otherwise                    = banana (left, right + n)

banana :: Pole -> Either String Pole
banana (left, right) = Left $ "Left:" ++ show left ++ ", Right:" ++ show right

routine :: Either String Pole
routine = do
  start <- return (0, 0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second
