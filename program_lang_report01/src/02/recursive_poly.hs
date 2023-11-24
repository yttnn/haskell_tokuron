recursive_poly :: [Int] -> Int -> Int
recursive_poly [] _ = 0
recursive_poly (x:xs) v = (x * v ^ (length xs)) + recursive_poly xs v