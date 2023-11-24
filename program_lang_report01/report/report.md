# プログラム言語特論 レポート1
author : 4555235023 丹野雄太

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
## 問題3
## 問題4
## 問題5
## 問題6
## 問題7
