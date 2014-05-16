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
    deriving (Show, Eq)

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
-- some bits from:
-- http://www.fatvat.co.uk/2009/09/graham-scan-in-haskell.html
sortCoord :: Point2D -> Point2D -> Ordering
sortCoord p1 p2
    | y1 == y2 = compare x1 x2
    | snd p1 < snd p2 = LT
    | otherwise = GT

sortPoints :: [Point2D] -> [Point2D]
sortPoints xs = sortBy sortCoord xs


--list of points?
testPoints1 = [(2.282154, 1.107697),
    (0.508136, 3.557593),
    (6.490489, 7.220862),
    (5.965358, 7.999035),
    (8.800029, 5.776057),
    (8.404778, 6.139152),
    (0.743481, 2.498024),
    (1.002515, 1.441651),
    (6.814331, 4.425290),
    (0.351636, 7.519613),
    (7.209313, 4.625614),
    (3.246308, 8.286591),
    (1.427197, 8.788250),
    (8.295495, 1.439363),
    (6.410940, 2.408661),
    (4.719379, 4.218519),
    (5.873703, 6.961398),
    (6.572547, 5.621853),
    (1.308362, 2.677804),
    (3.789837, 3.064397)]

{-- correct hull?
 [(2.282154, 1.107697),
(8.295495, 1.439363),
(8.800029, 5.776057),
(8.404778, 6.139152),
(5.965358, 7.999035),
(1.427197, 8.78825),
(0.351636, 7.519613),
(0.508136, 3.557593),
(0.743481, 2.498024),
(1.002515, 1.441651)]
--}






























