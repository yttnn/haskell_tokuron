myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile put []     = []
myTakeWhile put (x:xs) | put x = x : myTakeWhile put xs
                       | otherwise = []