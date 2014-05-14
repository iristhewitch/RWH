-- file: ch03/badexample.hs

badExample (x:xs) = x + badExample xs

goodExample (x:xs) = x + goodExample xs
goodExample _ = 0