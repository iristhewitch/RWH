-- file: ch04/exercises2.hs
import Data.Char (digitToInt)

-- 1
asInt_fold :: String -> Int
asInt_fold xs = foldl step 0 xs
    where step acc x = acc * 10 + digitToInt x

-- 2
-- material not taught; do later?

-- 3
myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs