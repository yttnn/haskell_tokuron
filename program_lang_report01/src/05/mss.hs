mss :: [Int] -> Int
mss [] = 0
mss (x:xs) = max x (x + mss xs)