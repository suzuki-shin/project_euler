module MyPrime (primes) where

primes :: Integer -> [Integer]
primes end = sieves' end (2:[3,5..end])

sieves' :: Integer -> [Integer] -> [Integer]
sieves' _ (n:[]) = [n]
sieves' end (n:ns)
  | end^2 < n = n:ns
  | otherwise = n:(sieves' (last nextList) nextList)
  where
    nextList = filter (\x -> (x `mod` n) /= 0) ns

