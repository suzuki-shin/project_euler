{-# OPTIONS_GHC -Wall #-}
-- 6 の約数は 1,2,3, そして 6 である.
-- これらの数の平方和は 1+4+9+36=50 となる.
-- n の約数の平方和を sigma2(n) で表すとしよう.
-- sigma2 の総和関数を SIGMA2 としよう, すなわち SIGMA2(n)=Σsigma2(i) ( i=1 から n まで ).
-- SIGMA2 の最初の6項は 1,6,16,37,63, そして 113 となる.
-- SIGMA2(10^15) modulo 10^9 を求めよ.

a :: Integer
-- a = 10^6
a = 10^15
b :: Integer
-- b = 10^2
b = 10^9

main :: IO ()
main = print $ fromInteger (sumSigma2 a b) `mod` b

-- 10^9の余りが必要なのだから、10^9を超えた部分はいらないはず
sumSigma2 :: Integer -> Integer -> Integer
sumSigma2 n m = foldl1 (\a b -> (sigma2' b m + a) `mod` m) [1..n]

-- | 約数の平方和を返す
-- >>> sigma2 1
-- 1
-- >>> sigma2 2
-- 5
-- >>> sigma2 6
-- 50
sigma2 :: Integer -> Integer
sigma2 n = sum $ map (^(2::Integer)) $ divisors n

-- | 最終的にmで割った余りを求めるのだから、各項をmで割った余りにしてよいはず
sigma2' :: Integer -> Integer -> Integer
sigma2' n m = sum $ map (\x -> (x^(2::Integer)) `mod` m) $ divisors n

-- | 約数のリストを返す
-- >>> divisors 1
-- [1]
-- >>> divisors 2
-- [1,2]
-- >>> divisors 6
-- [1,2,3,6]
divisors :: Integer -> [Integer]
divisors n = filter (\x -> (n `mod` x) == 0) [1..n]
