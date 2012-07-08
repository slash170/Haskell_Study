{-# OPTIONS -Wall -Werror #-}

import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

