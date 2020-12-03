module Main where

import Lib

main :: IO ()
main =  do
    day1data <- readFile "res/day1.txt"
    putStrLn ("Day 1 Part 1: " ++ (show . day1p1 . map read . lines $ day1data))
    putStrLn ("Day 1 Part 2: " ++ (show . day1p2 . map read . lines $ day1data))
    
    day2data <- readFile "res/day2.txt"
    putStrLn ("Day 2 Part 1: " ++ (show . day2p1 . lines $ day2data))
    putStrLn ("Day 2 Part 2: " ++ (show . day2p2 . lines $ day2data))
    
    day3data <- readFile "res/day3.txt"
    putStrLn ("Day 3 Part 1: " ++ (show . day3p1 3 1 . lines $ day3data))
    putStrLn ("Day 3 Part 2: " ++ (show . day3p2 [(1,1), (3,1), (5,1), (7,1), (1,2)] . lines $ day3data))
    
    putStrLn "tada!"
