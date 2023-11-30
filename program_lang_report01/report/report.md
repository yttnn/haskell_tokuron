---
title: レポート
date: 2020-05-07
author: frozenbonito
---
# プログラム言語特論 レポート1

## 目次
- [問題1](#問題1)
- [問題2](#問題2)
- [問題3](#問題3)
- [問題4](#問題4)
- [問題5](#問題5)
- [問題6](#問題6)
- [問題7](#問題7)

## 問題1
### myTake
```
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n [] = []
myTake n (x:xs) = x : myTake (n-1) xs
----------
ghci> myTake 3 [1,2,3,4,5]
[1,2,3]
ghci> myTake 0 [1,2,3,4,5]
[]
ghci> myTake 9 [1,2,3,4,5]
[1,2,3,4,5]
ghci> myTake 9 []
[]
```
- 強力なパターンマッチがあるため、ワイルドカード無いかなと思って探した所、あったのでつかってみた
- `myTake n [] = []`を忘れていたが、実行時エラーで気づいた
  - さすがにマッチ漏れまで検出はできないか
  - 関数自体をパターンマッチさせるから、コーナーケースを検査するの難しいのかなと、直感的に思った

### myTakeWhile
```
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile put []     = []
myTakeWhile put (x:xs) | put x = x : myTakeWhile put xs
                       | otherwise = []
-----------
ghci> myTakeWhile (<3) [1,2,3,4,5]
[1,2]
ghci> myTakeWhile (<9) [1,2,3,4,5]
[1,2,3,4,5]
ghci> myTakeWhile (<9) []
[]
```
- 定義を`myTakeWhile :: (Int -> Bool) -> [a] -> [a]`と間違えていた
  - コンパイラが`Int`と`a`の型不一致を指摘したため気づけた
- この時点で、今まで学んだ再帰関数の書き方と同じ考え方ができることに気づく
  - 再帰の停止地点（基底）と、内部処理の2つを書けばよいことを理解した

### myTakeDrop
```
myTakeDrop :: (a -> Bool) -> [a] -> [a]
myTakeDrop drop [] = []
myTakeDrop drop (x:xs) | drop x = myTakeDrop drop xs
                       | otherwise = x : myTakeDrop drop xs
---------------
ghci> myTakeDrop (<3) [1,2,3,4,5,6]
[3,4,5,6]
ghci> myTakeDrop (<0) [1,2,3,4,5,6]
[1,2,3,4,5,6]
ghci> myTakeDrop (<9) [1,2,3,4,5,6]
[]
ghci> myTakeDrop (<9) []
[]
```


## 問題2
### 再帰でpoly
```
recursive_poly :: [Int] -> Int -> Int
recursive_poly [] _ = 0
recursive_poly (x:xs) v = (x * v ^ (length xs)) + recursive_poly xs v
----------
ghci> recursive_poly [2,3,4] 2
18
ghci> recursive_poly [] 2
0
```
- 当たれられたリストの初項の値を計算し、後ろの項は再帰呼び出しすることで足していく
- 空リストが与えられたときは、0にすると仕様定義した
  - `recursive_poly [] _ = 0`が無くて、網羅的でないパターンだと怒られた

## 問題3
### merge3
- 2つのリストをくっつけるmeger2を作るという案で行く
  - merge2はx,yの大小で分岐し、x,yが同じ値の時は、2度同じ値を入れないように注意
- さらにそのmerge2を、merge3から2回呼ぶことで動作を実現する
```
merge2 :: Ord a => [a] -> [a] -> [a]
merge2 x [] = x
merge2 [] y = y
merge2 (x:xs) (y:ys) | x < y = x : merge2 xs (y:ys)
                     | x == y = x : merge2 xs ys
                     | otherwise = y : merge2 (x:xs) ys

merge3 :: Ord a => [a] -> [a] -> [a] -> [a]
merge3 x y z = merge2 x (merge2 y z)
-----------
ghci> merge3 [1,4,7] [2,5,8] [3,5,9]
[1,2,3,4,5,7,8,9]
ghci> merge3 [1,8,9] [1,2,3,4,7] [2,5,6,10]
[1,2,3,4,5,6,7,8,9,10]
```
- 3並列のときは、2並列を2回やるとよい、という教訓を活かせた

## 問題4
## 問題5
### 最大部分列の計算量
- 全探索すると、整数リストのi番目からj番目(i < j) までの和を順に求めるため、O(N^2)
- [Kadane's Algorithm](https://ajalab.github.io/2018/03/04/kadane.html)を使うとO(N)で解けるもよう
  - 再帰で綺麗に実装できそうな予感
### 最大部分積
- kadaneは適用できない気がするため、全探索で実装する

## 問題6
### (1)片側だけでもいい二分木を定義
- 片側しかない場合のBENode1を定義することで実現した
- 両側ある場合はBENode2とした
```
data BETree a = BELeaf a 
              | BENode1 a (BETree a)
              | BENode2 a (BETree a) (BETree a)
              deriving(Show)
```
### (2)3つの関数を作成
#### depthBETree
- 関数の目的は、引数の木t以下の深さを計算して返すこと
- Leafのときは、深さはカウントしない。
- Nodeの時は、深さカウントを1増やす
  - 今回は深さの最大値を求めたいため、Node2の時は大きい方の値を使用する
```
depthBETree :: BETree t -> Int
depthBETree (BELeaf a) = 0
depthBETree (BENode1 a t) = (depthBETree t) + 1
depthBETree (BENode2 a tl tr) = max (depthBETree tl + 1) (depthBETree tr + 1)
---------------
ghci> depthBETree (BENode2 5 (BENode2 8 (BELeaf 3) (BENode1 1 (BELeaf 7))) (BENode2 6 (BENode1 2 (BELeaf 9)) (BELeaf 4)))
3
```
#### sumBETree
- 関数の目的は、引数の木t以下の和を返すこと
- Leafの時は、足すものもないので、その値を返す
- Nodeの時は、自分の値を足しつつ、子のノードを再帰呼び出しし、その返り値を足す
```
sumBETree :: Num t => BETree t -> t
sumBETree (BELeaf a) = a
sumBETree (BENode1 a t) = a + (sumBETree t)
sumBETree (BENode2 a tl tr) = a + sumBETree tl + sumBETree tr
-------------
ghci> sumBETree (BENode2 5 (BENode2 8 (BELeaf 3) (BENode1 1 (BELeaf 7))) (BENode2 6 (BENode1 2 (BELeaf 9)) (BELeaf 4)))
45
```
#### upAccBETree
- 関数の目的は、引数の木t以下に所定の計算をすること
- Leafのときは、そのままLeafを返す
- Nodeの時は、引数の木以下の和に自分の値を足したものを自分の値にする。
- 更に、下にupAccを伝播させるために再帰呼び出しする
  - これを忘れていて、後輩の渡邉くんと野村くんに教えてもらった
```
upAccBETree :: Num t => BETree t -> BETree t
upAccBETree (BELeaf a) = (BELeaf a)
upAccBETree (BENode1 a t) = (BENode1 (a + (sumBETree t)) (upAccBETree t))
upAccBETree (BENode2 a tl tr) = (BENode2 (a + (sumBETree tl) + (sumBETree tr)) (upAccBETree tl) (upAccBETree tr))
-------
ghci> upAccBETree (BENode2 5 (BENode2 8 (BELeaf 3) (BENode1 1 (BELeaf 7))) (BENode2 6 (BENode1 2 (BELeaf 9)) (BELeaf 4)))
BENode2 45 (BENode2 19 (BELeaf 3) (BENode1 8 (BELeaf 7))) (BENode2 21 (BENode1 11 (BELeaf 9)) (BELeaf 4))
```
## 問題7
