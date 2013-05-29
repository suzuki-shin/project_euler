{-# OPTIONS_GHC -Wall #-}
-- | 完全数とは, その数の真の約数の和がそれ自身と一致する数のことである. たとえば, 28の真の約数の和は, 1 + 2 + 4 + 7 + 14 = 28 であるので, 28は完全数である.
-- 真の約数の和がその数よりも少ないものを不足数といい, 真の約数の和がその数よりも大きいものを過剰数と呼ぶ.
-- 12は, 1 + 2 + 3 + 4 + 6 = 16 となるので, 最小の過剰数である. よって2つの過剰数の和で書ける最少の数は24である. 
-- 数学的な解析により, 28123より大きい任意の整数は2つの過剰数の和で書けることが知られている. 2つの過剰数の和で表せない最大の数がこの上限よりも小さいことは分かっているのだが, この上限を減らすことが出来ていない.
-- 2つの過剰数の和で書き表せない正の整数の総和を求めよ.

data PerfectNum = Deficient | Perfect | Abundant deriving Show

perfectNum :: Integer -> PerfectNum
perfectNum n
  | perfect n < n = Deficient
  | perfect n > n = Abundant
  | otherwise     = Perfect
  where
    perfect :: Integer -> Integer
    perfect = sum . init . divisors

-- | 約数のリストを返す
divisors :: Integer -> [Integer]
divisors n = filter (\x -> (n `mod` x) == 0) [1..n]

-- | 28123以下の整数で2つの過剰数の和で書けないものは?
