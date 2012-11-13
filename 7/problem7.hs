{-# OPTIONS_GHC -Wall #-}
-- 素数を小さい方から6つ並べると 2, 3, 5, 7, 11, 13 であり、6番目の素数は 13 である。
-- 10001 番目の素数を求めよ。

main :: IO ()
main = print $ primes!!10000

end = 200000
primes = sieves' end (2:[3,5..end])

sieves' :: Integer -> [Integer] -> [Integer]
sieves' _ (n:[]) = [n]
sieves' end (n:ns)
  | end^2 < n = n:ns
  | otherwise = n:(sieves' (last nextList) nextList)
  where
    nextList = filter (\x -> (x `mod` n) /= 0) ns

