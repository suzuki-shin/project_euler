{-# OPTIONS_GHC -Wall #-}
-- | 問題
-- 10未満の自然数のうち、3 もしくは 5 の倍数になっているものは 3, 5, 6, 9 の4つがあり、 これらの合計は 23 になる。
-- 同じようにして、1,000 未満の 3 か 5 の倍数になっている数字の合計を求めよ。
num :: Integer
num = 1000

main = print $ sum $ multiOf3or5 num

multiOf3or5 :: Integer -> [Integer]
multiOf3or5 max = takeWhile (< max) $ filter (\n -> ((n `mod` 3) == 0 || (n `mod` 5) == 0)) [1..]