{-# OPTIONS_GHC -Wall #-}
-- 左右どちらから読んでも同じ値になる数を回文数という。 2桁の数の積で表される回文数のうち、最大のものは 9009 = 91 × 99 である。
-- では、3桁の数の積で表される回文数のうち最大のものはいくらになるか。

main = print $ maximum palindromes

palindromes = filter ((\x -> (x == reverse x)) . show) [a*b | a <- [100..999], b <- [100..999]]
