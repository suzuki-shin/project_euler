{-# OPTIONS_GHC -Wall #-}
-- 次の情報が与えられている.
-- 1900年1月1日は月曜日である.
-- 9月, 4月, 6月, 11月は30日まであり, 2月を除く他の月は31日まである.
-- 2月は28日まであるが, うるう年のときは29日である.
-- うるう年は西暦が4で割り切れる年に起こる. しかし, 西暦が400で割り切れず100で割り切れる年はうるう年でない.
-- 20世紀（1901年1月1日から2000年12月31日）中に月の初めが日曜日になるのは何回あるか?

type Year = Int
type Month = Int
type Day = Int

data Wday = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Enum, Bounded)
wdays :: [Wday]
wdays = cycle [minBound..maxBound]

main = print $ length $ filter isFirstDaySunday $ filter (\(y,_,_,_) -> y >= 1901) dateWdayPairs

dateWdayPairs :: [(Year, Month, Day, Wday)]
dateWdayPairs = zipWith (\(y,m,d) w -> (y,m,d,w)) [(y,m,d)|y<-[1900..2000],m<-[1..12],d<-[1..getLastDay y m]] wdays

isFirstDaySunday :: (Year, Month, Day, Wday) -> Bool
isFirstDaySunday (_,_,1,Sun) = True
isFirstDaySunday _ = False

getLastDay :: Year -> Month -> Int
getLastDay y 2
  | isLeapYear y = 29
  | otherwise = 28
getLastDay _ m
  | m `elem` [9,4,6,11] = 30
  | otherwise = 31

isLeapYear :: Int -> Bool
isLeapYear y = (y `mod` 4 == 0) && not (y `mod` 400 /= 0 && y `mod` 100 == 0)
