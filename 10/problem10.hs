{-# OPTIONS_GHC -Wall #-}
-- 10以下の素数の和は2 + 3 + 5 + 7 = 17である.
-- 200万以下の全ての素数の和を計算しなさい.

main :: IO ()
main = print $ sum $ primes 2000000

primes :: Integer -> [Integer]
primes end = 2:(sieve end [3,5..end])

sieve :: Integer -> [Integer] -> [Integer]
sieve end (n:ns)
  | end^(2::Integer) < n = n:ns
  | otherwise = n:(sieve (last nextList) nextList)
  where
    nextList = filter (\x -> ((x`mod`n) /= 0)) ns
sieve _ [] = []