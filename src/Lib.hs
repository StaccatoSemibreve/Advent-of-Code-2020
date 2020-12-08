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
    , day5p2v2
    , day6p1
    , day6p1v2
    , day6p2
    , day7p1
    , day7p2
    , day8p1
    , day8p2
    ) where

import Data.Char
import Data.List
import Data.Function
import Data.Maybe
import Data.List.Split
import Text.Read

day1p1 :: [Int] -> String
day1p1 ns = do
    let list = nub ns
    let table = [ (x+y, (x,y)) | x <- list, y <- (tail . dropWhile (/= x) $ list) ]
    maybe (error "heck (day 1p1.1)") (\(a,b) -> (show a) ++ "+" ++ (show b) ++ "= 2020, " ++ (show a) ++ "*" ++ (show b) ++ "=" ++ (show (a*b))) . lookup 2020 $ table

day1p2 :: [Int] -> String
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

day5p2v2 :: [String] -> Int
day5p2v2 seatBins = head . (\ids -> filter (not . (`elem` ids)) ([(minimum ids)..])) . map day5p1seatid $ seatBins

day6p1 :: String -> Int
day6p1 answers = sum . map (length . nub . concat . words) . splitOn "\n\n" $ answers

day6p1v2 :: String -> Int
day6p1v2 answers = sum . map (length . foldr1 union . lines) . splitOn "\n\n" $ answers

day6p2 :: String -> Int
day6p2 answers = sum . map (length . foldr1 intersect . lines) . splitOn "\n\n" $ answers

day7p1parse :: String -> (String, [(Int, String)])
day7p1parse line = case splitOn " contain " line of
                        [a, blist] -> case blist of
                                           "no other bags." -> (a, [])
                                           _ -> (maybe errorData id . stripSuffix " bags" $ a, map ((\(x:xs) -> (maybe (error $ "oh no " ++ (show (x:xs))) id . readMaybe $ x, intercalate " " xs)) . splitOn " ") . map (maybe errorData id . stripSuffixes [" bag", " bags"]) . splitOn ", " . init $ blist)
                        _ -> errorData
    where
        errorData = error $ "improperly formatted data: " ++ line
        
        stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
        stripSuffix suffix list = fmap reverse . (stripPrefix `on` reverse) suffix $ list
        
        stripSuffixes :: Eq a => [[a]] -> [a] -> Maybe [a]
        stripSuffixes suffixes list = case filter (not . isNothing) . fmap (`stripSuffix` list) $ suffixes of
                                           []       -> Nothing
                                           (a:_)    -> a

day7p1validate :: String -> [(String, [(Int, String)])] -> [(String, Bool)]
day7p1validate bag bagRules = map (bagElem bag bagRules) . map fst . filter (\rule -> (fst rule) /= bag) $ bagRules
    where
        bagElem :: String -> [(String, [(Int, String)])] -> String -> (String, Bool)
        bagElem _ [] checkBag = (checkBag, False)
        bagElem bag bagRules checkBag = do
            let subbags = maybe [] (map snd) . lookup checkBag $ bagRules
            if bag `elem` subbags
               then (checkBag, True)
               else (checkBag, or . map (snd . bagElem bag bagRules) $ subbags)

day7p1 :: [String] -> Int
day7p1 rules = length . filter snd . day7p1validate "shiny gold" . map day7p1parse $ rules

day7p2count :: String -> [(String, [(Int, String)])] -> Int
day7p2count bag bagRules = sum . map (\(count, subbag) -> (*count) . (+1) . day7p2count subbag $ bagRules) . maybe [] id . lookup bag $ bagRules

day7p2 :: [String] -> Int
day7p2 rules = day7p2count "shiny gold" . map day7p1parse $ rules

day8p1instruct :: [(String, Int)] -> [Int] -> Int -> Int
day8p1instruct instructions line acc = if or[(head line) `elem` (tail line), head line == length instructions]
                                                    then acc
                                                    else case instructions!!(head line) of
                                                              ("acc", arg)  -> day8p1instruct instructions (((+1) . head $ line):line) (acc+arg)
                                                              ("jmp", arg)  -> day8p1instruct instructions (((+arg) . head $ line):line) acc
                                                              ("nop", _)    -> day8p1instruct instructions (((+1) . head $ line):line) acc
                                                              _             -> error "invalid instruction"

day8p1parse :: String -> (String, Int)
day8p1parse = (\[a,b] -> (a, maybe (maybe 0 id . readMaybe . tail $ b) id . readMaybe $ b)) . words

day8p1 :: [String] -> Int
day8p1 instructions = day8p1instruct (map day8p1parse instructions) [0] 0

day8p2toggle :: Int -> [(String, Int)] -> [(String, Int)]
day8p2toggle line instructions = (take line instructions) ++ ((toggle $ instructions!!line):(drop (line+1) instructions))
    where
        toggle :: (String, Int) -> (String, Int)
        toggle ("jmp", arg) = ("nop", arg)
        toggle ("nop", arg) = ("jmp", arg)
        toggle x = x

day8p2instruct :: [(String, Int)] -> [Int] -> Int -> (Int, Bool)
day8p2instruct instructions line acc = if (head line) `elem` (tail line)
                                                    then (acc, False)
                                                    else if head line == length instructions
                                                        then (acc, True)
                                                        else case instructions!!(head line) of
                                                                ("acc", arg)  -> day8p2instruct instructions (((+1) . head $ line):line) (acc+arg)
                                                                ("jmp", arg)  -> day8p2instruct instructions (((+arg) . head $ line):line) acc
                                                                ("nop", _)    -> day8p2instruct instructions (((+1) . head $ line):line) acc
                                                                _             -> error "invalid instruction"

day8p2 ::[String] -> [Int]
day8p2 instructions = map fst . filter snd . map (\i -> day8p2instruct i [0] 0) . nub . (\i -> map (`day8p2toggle` i) [0..(length i - 1)]) $ map day8p1parse instructions
