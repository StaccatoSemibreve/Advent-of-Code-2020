module Lib
    ( day1p1, day1p2
    , day2p1, day2p2
    , day3p1, day3p2
    , day4p1, day4p2
    , day5p1, day5p2, day5p2v2
    , day6p1, day6p1v2, day6p2
    , day7p1, day7p2
    , day8p1, day8p2
    , day9p1, day9p2
    , day10p1, day10p2, day10p2bruteforce
    , day11p1, day11p2
    , day12p1, day12p2
    ) where

import Data.Char
import Data.List
import Data.Function
import Data.Maybe
import Data.Ix
import Data.List.Split
import Text.Read
import Data.Tree
import qualified Data.Map as Map

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
        bagElem bag bagRules checkBag = (\subbags -> (checkBag, or [bag `elem` subbags, or . map (snd . bagElem bag bagRules) $ subbags])) $ maybe [] (map snd) . lookup checkBag $ bagRules

day7p1 :: [String] -> Int
day7p1 rules = length . filter snd . day7p1validate "shiny gold" . map day7p1parse $ rules

day7p2count :: String -> [(String, [(Int, String)])] -> Int
day7p2count bag bagRules = sum . map (\(count, subbag) -> (*count) . (+1) . day7p2count subbag $ bagRules) . maybe [] id . lookup bag $ bagRules

day7p2 :: [String] -> Int
day7p2 rules = day7p2count "shiny gold" . map day7p1parse $ rules

data Day8Instruction = Acc Int | Jmp Int | Nop Int
    deriving (Eq, Show)
instance Read Day8Instruction where
    readsPrec _ input = do
        let parts = splitOn "," input
        [(parse . head $ parts, concat . tail $ parts)]
        where
            parse input = case splitAt 4 input of
                               ("acc ", arg) -> Acc (maybe (maybe 0 id . readMaybe . tail $ arg) id . readMaybe $ arg)
                               ("jmp ", arg) -> Jmp (maybe (maybe 0 id . readMaybe . tail $ arg) id . readMaybe $ arg)
                               ("nop ", arg) -> Nop (maybe (maybe 0 id . readMaybe . tail $ arg) id . readMaybe $ arg)
                               _            -> error "invalid instruction"

day8p1instruct :: [Day8Instruction] -> [Int] -> Int -> (Int, Bool)
day8p1instruct instructions line acc
    | (head line) ` elem` (tail line)   = (acc, False)
    | head line == length instructions  = (acc, True)
    | otherwise                         = case instructions!!(head line) of
                                               Acc arg -> day8p1instruct instructions (((+1) . head $ line):line) (acc+arg)
                                               Jmp arg -> day8p1instruct instructions (((+arg) . head $ line):line) acc
                                               Nop arg -> day8p1instruct instructions (((+1) . head $ line):line) acc

day8p1 :: [String] -> Int
day8p1 instructions = fst . day8p1instruct (map read instructions) [0] $ 0

day8p2toggle :: Int -> [Day8Instruction] -> [Day8Instruction]
day8p2toggle line instructions = (take line instructions) ++ ((toggle $ (instructions!!line)):(drop (line+1) instructions))
    where
        toggle :: Day8Instruction -> Day8Instruction
        toggle (Jmp arg) = Nop arg
        toggle (Nop arg) = Jmp arg
        toggle x = x

day8p2 :: [String] -> [Int]
day8p2 instructions = map fst . filter snd . map (\i -> day8p1instruct i [0] 0) . nub . (\i -> map (`day8p2toggle` i) [0..(length i - 1)]) $ map read instructions

day9p1zipper :: Int -> [a] -> [[a]]
day9p1zipper n list = [ take (min n x) . drop (max 0 (x-n)) $ list | x <- [0..length list] ]

day9p1 :: [Int] -> Int
day9p1 instructions = fst . head . filter (\(x,ys) -> not $ x `elem` ys) . drop 25 . zip instructions . map (\list -> [ x+y | (x:ys) <- tails list, y <- ys ]) . day9p1zipper 25 $ instructions

day9p2subsequences :: [Int] -> [[Int]]
day9p2subsequences list = concat . map subsequencePart $ [2..length list]
    where
        subsequencePart :: Int -> [[Int]]
        subsequencePart size = drop size . day9p1zipper size $ list

day9p2 :: [Int] -> Int
day9p2 instructions = (\xs -> (minimum xs) + (maximum xs)) . (\x -> head . dropWhile (\list -> sum list /= x) . day9p2subsequences $ instructions) . day9p1 $ instructions

day10p1 :: [Int] -> Int
day10p1 adapters = (\l -> (length . filter (== 1) $ l) * ((+1) . length . filter (== 3) $ l)) . (\l -> zipWith (-) (tail l) l) . sort $ 0:adapters

day10p2tree :: [Int] -> Tree Int
day10p2tree adapters = unfoldTree (\list -> (head list, filter (\l -> and[not . null $ l, (head l)-4 < head list]) . take 3 . tail . tails $ list)) $ (0 : sort adapters)

day10p2bruteforce :: [Int] -> Int
day10p2bruteforce adapters = foldTree (\_ counts -> if length counts == 0 then 1 else sum counts) . day10p2tree $ adapters

day10p2count :: [Int] -> Int
day10p2count [1,1,1,1] = 7
day10p2count [1,1,1] = 4
day10p2count [2,2] = 1
day10p2count [_,_] = 2
day10p2count [_] = 1
day10p2count [] = 1

day10p2 :: [Int] -> Int
day10p2 adapters = product . map day10p2count . splitOn [3] . (\l -> zipWith (-) (tail l) l) . sort $ 0:adapters

day11p1zipper :: [String] -> Map.Map (Int, Int) Char
day11p1zipper seats = Map.fromAscList . zip (range ((0,0), (length seats - 1, (length . head $ seats) - 1))) . concat $ seats

day11p1adjacents :: Map.Map (Int, Int) Char -> [[(Char, [Char])]]
day11p1adjacents seats = map (map snd) . groupBy ((==) `on` fst) $ [ (fst coord, (seat, adjacents)) | ((coord, seat), adjacents) <- zip (Map.toList seats) . map (map (\coords -> fromMaybe '.' . Map.lookup coords $ seats)) . map (\coords -> filter (/= coords) . range $ ((fst coords - 1, snd coords - 1), (fst coords + 1, snd coords + 1))) . Map.keys $ seats ]

day11p1iterateseat :: (Char, [Char]) -> Char
day11p1iterateseat ('L',adj)
    | null . filter (=='#') $ adj   = '#'
    | otherwise                     = 'L'
day11p1iterateseat ('#',adj)
    | (4>) . length . filter (=='#') $ adj  = '#'
    | otherwise                             = 'L'
day11p1iterateseat (x,_) = x

day11p1iterate :: [String] -> [String]
day11p1iterate = map (map day11p1iterateseat) . day11p1adjacents . day11p1zipper

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

day11p1 :: [String] -> Int
day11p1 seats = length . filter (=='#') . concat . converge (==) . iterate day11p1iterate $ seats

day11p2visibleline :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
day11p2visibleline (x,y) (r,d) = [ (x+r*n,y+d*n) | n <- [1..] ]

day11p2dirs :: [(Int,Int)]
day11p2dirs = [(-1,0),(-1,-1),(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1)]

day11p2getfirstseat :: Map.Map (Int,Int) Char -> (Int,Int) -> (Int,Int) -> Char
day11p2getfirstseat seats coords = case fromMaybe '.' . Map.lookup coords $ seats of
                                        '.' -> const '.'
                                        _ -> fromMaybe '.' . find (/= '.') . map (\c -> fromMaybe '?' . Map.lookup c $ seats) . day11p2visibleline coords

day11p2getfirstseats :: Map.Map (Int,Int) Char -> (Int,Int) -> [Char]
day11p2getfirstseats seats coords = map (day11p2getfirstseat seats coords) day11p2dirs

day11p2adjacents :: Map.Map (Int, Int) Char -> [[(Char, [Char])]]
day11p2adjacents seats = map (map snd) . groupBy ((==) `on` fst) $ [ (fst coord, (seat, adj)) | ((coord, seat), adj) <- zip (Map.toList seats) . map (day11p2getfirstseats seats) . Map.keys $ seats ]

day11p2iterateseat :: (Char, [Char]) -> Char
day11p2iterateseat ('L',adj)
    | null . filter (=='#') $ adj   = '#'
    | otherwise                     = 'L'
day11p2iterateseat ('#',adj)
    | (5>) . length . filter (=='#') $ adj  = '#'
    | otherwise                             = 'L'
day11p2iterateseat (x,_) = x

day11p2iterate :: [String] -> [String]
day11p2iterate = map (map day11p2iterateseat) . day11p2adjacents . day11p1zipper

day11p2 :: [String] -> Int
day11p2 seats = length . filter (=='#') . concat . converge (==) . iterate day11p2iterate $ seats

data Day12Direction = N | E | S | W
    deriving (Eq, Show, Enum)

data Day12Rotation = L Int | R Int
    deriving (Eq, Show)

data Day12Instruction = Card Day12Direction Int | Rot Day12Rotation | F Int
    deriving (Eq, Show)
instance Read Day12Instruction where
    readsPrec _ input = do
        let parts = lines input
        [(parse . head $ parts, concat . tail $ parts)]
        where
            parse input = case splitAt 1 input of
                               ("N", arg) -> Card N (fromMaybe (fromMaybe 0 . readMaybe . tail $ arg) . readMaybe $ arg)
                               ("S", arg) -> Card S (fromMaybe (fromMaybe 0 . readMaybe . tail $ arg) . readMaybe $ arg)
                               ("E", arg) -> Card E (fromMaybe (fromMaybe 0 . readMaybe . tail $ arg) . readMaybe $ arg)
                               ("W", arg) -> Card W (fromMaybe (fromMaybe 0 . readMaybe . tail $ arg) . readMaybe $ arg)
                               ("L", arg) -> Rot $ L (fromMaybe (fromMaybe 0 . readMaybe . tail $ arg) . readMaybe $ arg)
                               ("R", arg) -> Rot $ R (fromMaybe (fromMaybe 0 . readMaybe . tail $ arg) . readMaybe $ arg)
                               ("F", arg) -> F (fromMaybe (fromMaybe 0 . readMaybe . tail $ arg) . readMaybe $ arg)
                               _          -> error "invalid instruction"

data Day12State = Day12State Day12Direction (Int,Int)
      deriving (Eq, Show)

day12p1rotate :: Day12Rotation -> Day12Direction -> Day12Direction
day12p1rotate (R 0) dir = dir
day12p1rotate (R 90) W = N
day12p1rotate (R 90) dir = succ dir
day12p1rotate (R x) dir = (!!((x `mod` 360) `quot` 90)) . iterate (day12p1rotate (R 90)) $ dir
day12p1rotate (L x) dir = day12p1rotate (R (360-x)) dir

day12p1move :: Day12Instruction -> Day12State -> Day12State
day12p1move (Card N d) (Day12State dir (x,y)) = Day12State dir (x,y+d)
day12p1move (Card E d) (Day12State dir (x,y)) = Day12State dir (x+d,y)
day12p1move (Card S d) (Day12State dir (x,y)) = Day12State dir (x,y-d)
day12p1move (Card W d) (Day12State dir (x,y)) = Day12State dir (x-d,y)
day12p1move (Rot r) (Day12State dir pos) = Day12State (day12p1rotate r dir) pos
day12p1move (F d) s@(Day12State dir pos) = day12p1move (Card dir d) s

day12p1manhattan :: Day12State -> Int
day12p1manhattan (Day12State _ (x,y)) = ((+) `on` abs) x $ y

day12p1 :: [String] -> Int
day12p1 instrs = day12p1manhattan . foldr ($) (Day12State E (0,0)) . map (day12p1move . read) . reverse $ instrs

data Day12Ship = Day12Ship (Int, Int) (Int, Int)
      deriving (Eq, Show)

day12p2rotate :: Day12Rotation -> (Int,Int) -> (Int,Int)
day12p2rotate (R 0) dir = dir
day12p2rotate (R 90) (x,y) = (y,-x)
day12p2rotate (R x) dir = (!!((x `mod` 360) `quot` 90)) . iterate (day12p2rotate (R 90)) $ dir
day12p2rotate (L x) dir = day12p2rotate (R (360-x)) dir

day12p2move :: Day12Instruction -> Day12Ship -> Day12Ship
day12p2move (Card N d) (Day12Ship (x,y) pos) = Day12Ship (x,y+d) pos
day12p2move (Card E d) (Day12Ship (x,y) pos) = Day12Ship (x+d,y) pos
day12p2move (Card S d) (Day12Ship (x,y) pos) = Day12Ship (x,y-d) pos
day12p2move (Card W d) (Day12Ship (x,y) pos) = Day12Ship (x-d,y) pos
day12p2move (Rot r) (Day12Ship wp pos) = Day12Ship (day12p2rotate r wp) pos
day12p2move (F d) (Day12Ship wp@(dx,dy) (x,y)) = Day12Ship wp (x+d*dx, y+d*dy)

day12p2manhattan :: Day12Ship -> Int
day12p2manhattan (Day12Ship _ (x,y)) = ((+) `on` abs) x $ y

day12p2 :: [String] -> Int
day12p2 instrs = day12p2manhattan . foldr ($) (Day12Ship (10,1) (0,0)) . map (day12p2move . read) . reverse $ instrs
