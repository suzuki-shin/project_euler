{-# OPTIONS_GHC -Wall #-}
-- | 問題
-- フィボナッチ数列の項は前の2つの項の和である。 最初の2項を 1, 2 とすれば、最初の10項は以下の通りである。
--  1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
-- 数列の項の値が400万を超えない範囲で、偶数値の項の総和を求めよ。

main = print $ sum $ takeWhile (<4000000) $ filter even $ fib 1 2

fib :: Num a => a -> a -> [a]
fib n1 n2 = fib'
  where
    fib' = n1 : n2 : zipWith (+) fib' (tail fib')