-- file: ch02/lastButOne.hs

lastButOne :: [a] -> a
lastButOne xs = head (drop (length xs - 2) xs)