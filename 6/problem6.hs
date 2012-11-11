{-# OPTIONS_GHC -Wall #-}
-- 最初の10個の自然数について、その和の二乗と、二乗数の和は以下の通り。
-- 1² + 2² + ... + 10² = 385
-- (1 + 2 + ... + 10)² = 3025
-- これらの数の差は 3025 - 385 = 2640 となる。
-- 同様にして、最初の100個の自然数について和の二乗と二乗の和の差を求めよ。

squareSum :: [Integer] -> Integer
squareSum ns = sum $ map (^2) ns

sumSquare :: [Integer] -> Integer
sumSquare ns = (sum ns) ^ 2

main = print $ (sumSquare [1..100]) - (squareSum [1..100])