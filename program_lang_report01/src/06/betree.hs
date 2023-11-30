data BETree a = BELeaf a 
              | BENode1 a (BETree a)
              | BENode2 a (BETree a) (BETree a)
              deriving(Show)

depthBETree :: BETree t -> Int
depthBETree (BELeaf a) = 0
depthBETree (BENode1 a t) = (depthBETree t) + 1
depthBETree (BENode2 a tl tr) = max (depthBETree tl + 1) (depthBETree tr + 1)

sumBETree :: Num t => BETree t -> t
sumBETree (BELeaf a) = a
sumBETree (BENode1 a t) = a + (sumBETree t)
sumBETree (BENode2 a tl tr) = a + sumBETree tl + sumBETree tr

upAccBETree :: Num t => BETree t -> BETree t
upAccBETree (BELeaf a) = (BELeaf a)
upAccBETree (BENode1 a t) = (BENode1 (a + (sumBETree t)) (upAccBETree t))
upAccBETree (BENode2 a tl tr) = (BENode2 (a + (sumBETree tl) + (sumBETree tr)) (upAccBETree tl) (upAccBETree tr))

-- (BENode2 5 (BENode2 8 (BELeaf 3) (BENode1 1 (BELeaf 7))) (BENode2 6 (BENode1 2 (BELeaf 9)) (BELeaf 4)))