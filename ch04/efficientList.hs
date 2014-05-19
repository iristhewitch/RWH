-- file: ch04/efficientList.hs

myDumbExample xs = if length xs > 0
                   then head xs
                   else 'z'

mySmartExample xs = if not (null xs)
                    then head xs
                    else 'z'

-- a second way
--mySmartExample (x:_) = x
--mySmartExample [] = 'z'