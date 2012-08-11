{-# OPTIONS -Wall #-}

import qualified Data.Map as Map

data Person = Person { firstName :: String,
                       lastName :: String,
                       age :: Int
                     } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wendsday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

mikeD = Person {firstName = "Michel", lastName="Diamond", age=43}
adRock = Person {firstName = "Adam", lastName="Horovitz", age=41}
mca = Person {firstName = "Adam", lastName="Yauch", age=44}

mysteryDude = "Person {firstName = \"Adam\"" ++
              ", lastName=\"Yauch\"" ++
              ", age=44}"

-- 型シノニム
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]
    
phoneBook :: PhoneBook
phoneBook =
    [ ("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k, v)]

-- LockerState
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap
lockers = Map.fromList
          [(100, (Taken, "ZD39I")),
           (101, (Free,  "JAH3I")),
           (103, (Free,  "IQSA9")),
           (105, (Free,  "QOTSA")),
           (109, (Taken, "893JJ")),
           (110, (Taken, "99292"))
          ]

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
                                  Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
                                  Just (state, code) -> if state /= Taken
                                                        then Right code
                                                        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
-- 7.7
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

infixr 5 :-:
data List' a = Empty' | a :-: (List' a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List' a -> List' a -> List' a
Empty' ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

-- 2分探索木
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right