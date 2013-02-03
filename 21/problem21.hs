{-# OPTIONS_GHC -Wall #-}
-- d(n) を n の真の約数の和と定義する. (真の約数とは n 以外の約数のことである. )
-- もし, d(a) = b かつ d(b) = a (a ≠ b のとき) を満たすとき, a と b は友愛数(親和数)であるという.

-- 例えば, 220 の約数は 1, 2, 4, 5, 10, 11, 20, 22, 44, 55, 110 なので d(220) = 284 である.
-- また, 284 の約数は 1, 2, 4, 71, 142 なので d(284) = 220 である.

-- それでは10000未満の友愛数の和を求めよ.

import Data.Maybe

-- | 約数のリストを返す
-- >>> divisors 1
-- [1]
-- >>> divisors 2
-- [1,2]
-- >>> divisors 6
-- [1,2,3,6]
divisors :: Integer -> [Integer]
divisors n = filter (\x -> (n `mod` x) == 0) [1..n]

sumTrueDivisors :: Integer -> Integer
sumTrueDivisors 0 = 0
sumTrueDivisors n = sum $ init $ divisors n

friendNum :: Integer -> Maybe Integer
friendNum n
  | sumTrueDivisors m == n && m /= n = Just m
  | otherwise = Nothing
  where
    m = sumTrueDivisors n

main :: IO ()
main = print $ sum $ catMaybes $ map friendNum [1..9999]