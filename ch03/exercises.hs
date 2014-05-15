-- file: ch03/exercises.hs
import Data.List (sortBy)


-- 1,2
numElements :: [a] -> Int
numElements [] = 0
numElements (x:xs) = 1 + numElements xs

-- 3
sumElements :: Num a => [a] -> a
sumElements [] = 0
sumElements (x:xs) = x + sumElements xs

meanElements xs = (sumElements xs) / (fromIntegral (numElements xs))

-- 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

palindromeList :: [a] -> [a]
palindromeList [] = []
palindromeList xs = xs ++ (myReverse xs)

-- 5
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- 6
sortLength :: [a] -> [a] -> Ordering
sortLength xs ys
    | length xs > length ys = GT
    | length xs < length ys = LT
    | length xs == length ys = EQ

sortLists :: [[a]] -> [[a]]
sortLists xs = sortBy sortLength xs

-- 7
myIntersperse :: a -> [[a]] -> [a]
myIntersperse i [] = []
myIntersperse i (xs:[]) = xs
myIntersperse i (x:xs) = x ++ i : (myIntersperse i xs)

-- 8
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving Show

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)
    where max a b = if a > b then a else b

-- 9
data Direction = CW | CCW | COL
    deriving (Show)

-- 10
type Point2D = (Double, Double)

direction :: Point2D -> Point2D -> Point2D -> Direction
direction a b c
    | d > 0  = CCW
    | d < 0  = CW
    | d == 0 = COL
   where d = ((fst b - fst a) * (snd c - snd a)) - ((snd b - snd a)*(fst c - fst a))

-- 11
directions (a:b:c:xs) = (direction a b c) : (directions (b:c:xs))
directions (a:b:[]) = []
directions [] = []

-- 12
graham (x:xs)

























