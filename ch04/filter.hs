-- file: ch04/filter.hs

oddList :: [Int] -> [Int]
oddList (x:xs)
    | odd x     = x : oddList xs
    | otherwise = oddList xs
oddList _       = []