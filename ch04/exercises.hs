-- file: ch04/exercises.hs

-- 1
safeHead :: [a] -> Maybe a
safeHead xs = if not (null xs)
              then Just (head xs)
              else Nothing

safeTail :: [a] -> Maybe [a]
safeTail xs = if not (null xs)
              then Just (tail xs)
              else Nothing

safeLast :: [a] -> Maybe a
safeLast xs = if not (null xs)
              then Just (last xs)
              else Nothing

safeInit :: [a] -> Maybe [a]
safeInit xs = if not (null xs)
              then Just (init xs)
              else Nothing

-- 2
-- current answer from the comments
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = [[]]
splitWith pred (x:xs)
    | pred x == False = [x]:rest
    | pred x == True = [x:head rest] ++ tail rest
        where rest = splitWith pred xs

-- 3
-- in exercise3.hs