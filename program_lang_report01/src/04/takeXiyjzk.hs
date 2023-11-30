takeAs :: Int -> Int -> Int -> Int -> [Int] -> [Int] -> [Int] -> [Int]
takeAs 0 _ _ _ _ _ _ = [1]
takeAs k a b c x y z = merge2 (merge3 x y z) (takeAs (k-1) a b c (map (* a) (merge3 x y z))
                                                                 (map (* b) (merge3 x y z))
                                                                 (map (* c) (merge3 x y z)))

takeXiyjzk :: Int -> Int -> Int -> Int -> [Int]
takeXiyjzk 0 x y z = []
takeXiyjzk k x y z = take k (dropWhile (< 1) (takeAs k x y z [1] [1] [1]))

merge2 :: Ord a => [a] -> [a] -> [a]
merge2 x [] = x
merge2 [] y = y
merge2 (x:xs) (y:ys) | x < y = x : merge2 xs (y:ys)
                     | x == y = x : merge2 xs ys
                     | otherwise = y : merge2 (x:xs) ys

merge3 :: Ord a => [a] -> [a] -> [a] -> [a]
merge3 x y z = merge2 x (merge2 y z)