{-# OPTIONS_GHC -Wall #-}
-- 2^15 = 32768 であり, これの各桁の和は 3 + 2 + 7 + 6 + 8 = 26 となる.
-- 同様にして, 2^1000 の各桁の和を求めよ.
import Data.Char

main = print $ sum $ map digitToInt $show $ 2^1000

