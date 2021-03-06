{-# OPTIONS -Werror #-}
doubleMe :: Num a => a -> a
doubleMe x = x + x
doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100
	then x
	else x * 2
doubleSmallNumber' :: (Ord a, Num a) => a -> a
doubleSmallNumber' x = ( if x > 100 then x else x * 2 ) + 1

conanO'Brien :: String
conanO'Brien = "It's a-me, Conan O'Brien!"

removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [ c | c <- st, c `elem` ['A'..'Z']]
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]
circumference :: Float -> Float
circumference r = 2 * pi * r
circumference' :: Double -> Double
circumference' r = 2 * pi * r

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!" ++ show x

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5" ++ show x

factorial' :: Int -> Int
factorial' 0 = 1
factorial' n = n * factorial' ( n - 1 )

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName x = "you wrong" ++ " " ++ show x


addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
first :: (a,b,c) -> a
first (x,_,_) = x
second :: (a,b,c) -> b
second (_,y,_) = y
third :: (a,b,c) -> c
third (_,_,z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x


tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two element: " ++ show x ++ " and " ++ show y
tell (x:y:_)  = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"
  | otherwise   = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b      = b
  | otherwise   = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b      = EQ
  | a <= b      = LT
  | otherwise   = GT

badGreeting :: String
badGreeting = "Oh! Pfft. It's you."
niceGreeting :: String
niceGreeting = "Hello! SO very nice to see you,"

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w/h^2, bmi > 25.0]

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea  = pi * r ^ 2
  in  sideArea + 2 * topArea


head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x

describeList :: [a] -> String
describeList ls = "The list is "
                  ++ case ls of [] -> "empty."
                                [x] -> "a singleton list"
                                xs -> "a longer list"

describeList'' :: [a] -> String
describeList'' ls = "The list is " ++ what ls
  where what [] = "empty."
        what [x] = "a singleton list."
        what xs  = "a longer list."




