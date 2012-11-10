{-# OPTIONS_GHC -Wall #-}
-- 13195 の素因数は 5、7、13、29 である。
-- 600851475143 の素因数のうち最大のものを求めよ。
num :: Integer
-- num = 13195
num = 600851475143
main :: IO ()
main = print $ maximum $ primeFactors num

-- ステップ 1 [編集]
-- 整数を最初の素数である 2 から昇順で探索リストに羅列する。
-- 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
-- ステップ 2 [編集]
-- リストの先頭の数を素数リストに記録する。
-- 素数リスト：2
-- 探索リスト：2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
-- ステップ 3 [編集]
-- 前のステップで素数リストに加えられた数の全ての倍数を、探索リストから削除する。
-- 素数リスト：2
-- 探索リスト：3 5 7 9 11 13 15 17 19
-- ステップ 4 [編集]
-- 探索リストの最大値が素数リストの最大値の平方よりも小さい場合、素数リストおよび探索リストに残っている数が素数となる。探索リストの最大値が素数リストの最大値の平方よりも大きい場合、ステップ 2 に戻る。
primes :: Integer -> [Integer]
primes end = sieve end (2:[3,5..])

sieve :: Integer -> [Integer] -> [Integer]
sieve end (n:ns) = n:(sieve end $ takeWhile (\x -> end >= x^2) $ filter (\x -> (x `mod` n) /= 0) ns)
sieve _ [] = []

primeFactors :: Integer -> [Integer]
primeFactors n = filter (\x -> (n `mod` x) == 0) $ primes n