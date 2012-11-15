{-# OPTIONS_GHC -Wall #-}
-- ピタゴラスの三つ組(ピタゴラスの定理を満たす自然数)とはa<b<cで
-- a² + b² = c²
-- を満たす数の組である.
-- 例えば, 3² + 4² = 9 + 16 = 25 = 5²である.
-- a + b + c = 1000となるピタゴラスの三つ組が一つだけ存在する. このa,b,cの積を計算しなさい.

main :: IO ()
main = print $ product $ head $ pythagoras 1000

pythagoras :: Integer -> [[Integer]]
pythagoras num = [[a,b,c]|c <- [5..(num-2)], b <- [1..c], a <- [1..b], a+b+c == num, a^(2::Integer)+b^(2::Integer) == c^(2::Integer)]
