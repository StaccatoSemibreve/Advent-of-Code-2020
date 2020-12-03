module Lib
    ( day1p1
    , day1p2
    , day2p1
    , day2p2
    , day3p1
    , day3p2
    ) where

import Data.List

day1p1 :: [Integer] -> String
day1p1 ns = do
    let list = nub ns
    let table = [ (x+y, (x,y)) | x <- list, y <- (tail . dropWhile (/= x) $ list) ]
    maybe (error "heck (day 1p1.1)") (\(a,b) -> (show a) ++ "+" ++ (show b) ++ "= 2020, " ++ (show a) ++ "*" ++ (show b) ++ "=" ++ (show (a*b))) . lookup 2020 $ table

day1p2 :: [Integer] -> String
day1p2 ns = do
    let list = nub ns
    let table = [ (x+y+z, (x,y,z)) | x <- list, y <- (tail . dropWhile (/= x) $ list), z <- (tail . dropWhile (/= y) $ list) ]
    maybe (error "heck (day 1p2.1)") (\(a,b,c) -> (show a) ++ "+" ++ (show b) ++ "+" ++ (show c) ++ "= 2020, " ++ (show a) ++ "*" ++ (show b) ++ "*" ++ (show c) ++ "=" ++ (show (a*b*c))) . lookup 2020 $ table
    
day2p1 :: [String] -> Int
day2p1 list = length . filter (\(mincount, maxcount, count) -> (count >= mincount) && (count <= maxcount)) . map (\(mincount, maxcount, c, text) -> (mincount, maxcount, length . filter (== c) $ text)) . map (\s -> (\parts -> (read . takeWhile (/= '-') $ (parts!!0), read . tail . dropWhile (/= '-') $ (parts!!0), head (parts!!1), parts!!2)) $ words s) $ list

day2p2 list = length . filter id . map (\(pos1, pos2, c, text) -> ((text!!(pos1-1)) == c) /= ((text!!(pos2-1)) == c)) . map (\s -> (\parts -> (read . takeWhile (/= '-') $ (parts!!0), read . tail . dropWhile (/= '-') $ (parts!!0), head (parts!!1), parts!!2)) $ words s) $ list

day3p1 :: Int -> Int -> [String] -> Int
day3p1 right down treemap = length . filter (== '#') . map (\(n, s) -> (cycle s)!!(n*right)) . zip [0..] . map snd . filter (\(n,_) -> (n `mod` down == 0)) . zip [0..] $ treemap

day3p2 :: [(Int, Int)] -> [String] -> Int
day3p2 paths treemap = product . map (\(right, down) -> day3p1 right down treemap) $ paths
