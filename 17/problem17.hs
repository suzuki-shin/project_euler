{-# OPTIONS_GHC -Wall #-}
-- | 1 から 5 までの数字を英単語で書けば one, two, three, four, five であり, 全部で 3 + 3 + 5 + 4 + 4 = 19 の文字が使われている.
-- では 1 から 1000 (one thousand) までの数字をすべて英単語で書けば, 全部で何文字になるか.
-- 注: 空白文字やハイフンを数えないこと. 例えば, 342 (three hundred and forty-two) は 23 文字, 115 (one hundred and fifteen) は20文字と数える. なお, "and" を使用するのは英国の慣習.

import Data.Char (isAlpha)

units :: [String]
units = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens :: [String]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens :: [String]
tens =  ["twenty", "thirty", "forty", "fifty","sixty", "seventy", "eighty", "ninety"]
thousand :: [String]
thousand = ["thousand"]

convert2 :: Int -> String
convert2 = combine2 . digit2

digit2 :: Int -> (Int, Int)
digit2 n = (n `div` 10, n `mod` 10)

combine2 :: (Int, Int) -> String
combine2 (0, u)
  | u >= 1 = units!!(u-1) -- combine2 (0, u + 1) = units!!u -- これのu + 1はhaskell的にはだめだった
  | otherwise = error "error"
combine2 (1, u) = teens!!u
combine2 (t, 0)
  | t >= 2 = tens!!(t-2)
  | otherwise = error "error"
combine2 (t, u)
  | t >= 2 = tens!!(t-2) ++ "-" ++ units!!(u-1)
  | otherwise = error "error"

convert3 :: Int -> String
convert3 = combine3 . digit3

digit3 :: Int -> (Int, Int)
digit3 n = (n`div`100, n`mod`100)

combine3 :: (Int, Int) -> String
combine3 (0,t)
  | t >= 1 = convert2 t
  | otherwise = error "error"
combine3 (h,0)
  | h >= 1 = units!!(h-1) ++ " hundred"
  | otherwise = error "error"
combine3 (h,t)
  | h >= 1 && t >= 1 = units!!(h-1) ++ " hundred and " ++ convert2 t
  | otherwise = error "error"

convert :: Int -> String
convert 1000 = "one thousand"
convert n = convert3 n

countLetter :: String -> Int
countLetter = length . (filter isAlpha)

main = print $ sum $ map (countLetter . convert) [1..1000]
