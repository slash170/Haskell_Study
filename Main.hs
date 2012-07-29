{-# OPTIONS -Wall #-}

import Shapes
main = do
--  print $ Circle (Point 10 20) 30
  print $ nudge (baseCircle 30) 10 20