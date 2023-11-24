foldl_poly :: [Int] -> Int -> Int
foldl_poly (x:xs) v = foldl f (0) (x:xs)
                      where f x n = x * v ^ (length xs) + n