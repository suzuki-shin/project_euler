{-# OPTIONS_GHC -Wall #-}
data Direct = RightD | DownD deriving (Show, Eq)
type Root = [Direct]

main :: IO ()
main = print $ length $ roots 20 20

-- 横nマス、縦mマスのマス目
roots :: Int -> Int -> [Root]
roots n m = validRoot n $ mapM (\_ -> [RightD, DownD]) [1..(n*m)]

-- 横nマス分ちょうど移動という縛り
validRoot :: Int -> [[Direct]] -> [[Direct]]
validRoot n = filter (\root -> length (filter (\d -> d == RightD) root) == n)