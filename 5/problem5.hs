{-# OPTIONS_GHC -Wall #-}
-- 2520 は 1 から 10 の数字の全ての整数で割り切れる数字であり、そのような数字の中では最小の値である。
-- では、1 から 20 までの整数全てで割り切れる数字の中で最小の値はいくらになるか。

isDividableBy1To :: Int -> Int -> Bool
isDividableBy1To n maxDiv = all (\x -> (n `mod` x) == 0) [1..maxDiv]

main = print $ head $ filter (`isDividableBy1To` 20) [1..]