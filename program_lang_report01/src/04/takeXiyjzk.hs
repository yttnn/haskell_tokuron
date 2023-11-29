takeXiyjzk :: Int -> Int -> Int -> Int -> [Int]
takeXiyjzk 0 x y z = []
takeXiyjzk k x y z = 

generateXiyjzk :: Int -> Int -> Int -> Int -> Int -> Int -> [Int]
generateXiyjzk x i y j z k = [(x^i) * (y^j) * (z^k)] ++ 