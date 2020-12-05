module Lib
    ( day1p1
    , day1p2
    , day2p1
    , day2p2
    , day3p1
    , day3p2
    , day4p1
    , day4p2
    , day5p1
    , day5p2
    ) where

import Data.Char
import Data.List
import Data.List.Split
import Text.Read

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

day4p1validate :: [(String, String)] -> Bool
day4p1validate passport = (== 0) . length $ ["byr","iyr","eyr","hgt","hcl","ecl","pid"] \\ map fst passport

day4p1 :: String -> Int
day4p1 passports = length . filter day4p1validate . map (map ((\(a:b:_) -> (a,b)) . (splitOn ":"))) . map words . splitOn "\n\n" $ passports

day4p2validate :: (String, String) -> Bool
day4p2validate ("byr", val) = and [length val == 4, maybe False (>= 1920) . readMaybe $ val, maybe False (<= 2002) . readMaybe $ val]
day4p2validate ("iyr", val) = and [length val == 4, maybe False (>= 2010) . readMaybe $ val, maybe False (<= 2020) . readMaybe $ val]
day4p2validate ("eyr", val) = and [length val == 4, maybe False (>= 2020) . readMaybe $ val, maybe False (<= 2030) . readMaybe $ val]
day4p2validate ("hgt", val) = case span isDigit val of
                                   (num, "cm")  -> and [maybe False (>= 150) . readMaybe $ num, maybe False (<= 193) . readMaybe $ num]
                                   (num, "in")  -> and [maybe False (>= 59) . readMaybe $ num, maybe False (<= 76) . readMaybe $ num]
                                   _            -> False
day4p2validate ("hcl", ('#':val)) = and [length val == 6, (== 0) . length . filter (`notElem` "0123456789abcdef") $ val]
day4p2validate ("ecl", val) = val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
day4p2validate ("pid", val) = and [length val == 9, (== 0) . length . filter (`notElem` "0123456789") $ val]
day4p2validate ("cid", _) = True
day4p2validate _ = False

day4p2 :: String -> Int
day4p2 passports = length . filter (\passport -> and [day4p1validate passport, and . map day4p2validate $ passport]) . map (map ((\(a:b:_) -> (a,b)) . (splitOn ":"))) . map words . splitOn "\n\n" $ passports

day5p1seatid :: String -> Int
day5p1seatid binString = (\[a,b] -> 8*a+b) . map (sum . zipWith (*) (iterate (*2) 1) . reverse . map (fromEnum . (`elem` "BR"))) . (\(a,b) -> [a,b]) . splitAt 7 $ binString

day5p1 :: [String] -> Int
day5p1 seatBins = maximum . map day5p1seatid $ seatBins

day5p2 :: [String] -> Int
day5p2 seatBins = (+1) . fst . head . filter (\(a,b) -> (b-a) /= 1) . (\list -> zip list $ tail list) . sort . map day5p1seatid $ seatBins
