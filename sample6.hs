{-# OPTIONS -Wwarn -Werror #-}

import Data.List
import Data.Char
import qualified Data.Map as Map
import Geometry
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cube as Cube
import qualified Geometry.Cuboid as Cuboid

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String,Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
        | key == k = Just v
        | otherwise = findKey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key xs = foldr
                  (\(k, v) acc -> if key == k then Just v else acc)
                  Nothing xs

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ ", " ++ number2

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs

cubeVolume3 :: Float
cubeVolume3 = cubeVolume 3

cubeArea4 :: Float
cubeArea4 = cubeArea 4

cubeVolume3' :: Float
cubeVolume3' = Cube.volume 3

cubeArea4' :: Float
cubeArea4' = Cube.area 4
               
-- Data
phoneBook :: Map.Map String String
phoneBook = Map.fromList $
            [ ("betty","555-2938")
             ,("bonnie","452-2928")
             ,("patsy","493-2928")
             ,("lucille","205-2928")
             ,("wendy","939-8282")
             ,("penny","853-2492")
             ]

phoneBook' =
    [ ("betty","555-2938")
     ,("betty","342-2492")
     ,("bonnie","452-2928")
     ,("patsy","493-2928")
     ,("patsy","943-2929")
     ,("patsy","827-9162")
     ,("lucille","205-2928")
     ,("wendy","939-8282")
     ,("penny","853-2492")
     ,("penny","555-2111")
    ]

