module Main where

import Data.List (sort)

main :: IO ()
main = do
  content <- readFile "./problems/22-name-scores/names.txt"
  let names = sort $ words content
  print $ score 1 names

score :: Int -> [String] -> Int
score _ [] = 0
score idx (x : xs) = (idx * nameScore x) + score (idx + 1) xs

nameScore :: String -> Int
nameScore str = sum $ map (\x -> fromEnum x - fromEnum 'A' + 1) str
