{-# OPTIONS_GHC -Wall #-}
-- n × (n - 1) × ... × 3 × 2 × 1 を n! と表す.
-- 例えば, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800 となる.
-- この数の各桁の合計は 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27 である.
-- では, 100! の各桁の数字の和を求めよ.
import Data.Char

factorial n = product [1..n]
digitSum n = sum $ map digitToInt $ show n

main = print $ digitSum $ factorial 100