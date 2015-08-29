import Data.List (find)
import Data.Maybe (fromJust)
import Data.Function.Memoize (memoize)

main :: IO ()
main = print $ question 500

triangularNumber :: Int -> Int
triangularNumber 1 = 1
triangularNumber n = n + triangularNumber (n - 1)

triangularNumberMemo = memoize triangularNumber


factorsOf :: Int -> (Int ,[Int])
factorsOf n
  | n > 2 = (n, 1 : filter (\m -> n `mod` m == 0) [2..n-1])
  | n == 2 = (n, [1,2])
  | n == 1 = (n, [1])
  | otherwise = error "xxx"

question :: Int -> (Int, [Int])
question num =
  fromJust $ find (\(_, x) -> length x >= num) $ map (factorsOf . triangularNumberMemo) [1..]
