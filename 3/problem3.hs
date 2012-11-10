{-# OPTIONS_GHC -Wall #-}
import Data.Numbers.Primes
-- 13195 の素因数は 5、7、13、29 である。
-- 600851475143 の素因数のうち最大のものを求めよ。
num :: Integer
-- num = 13195
num = 600851475143
main :: IO ()
main = print $ maximum $ primeFactors num
