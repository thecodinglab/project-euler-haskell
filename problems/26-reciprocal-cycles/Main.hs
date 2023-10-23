module Main where

import Data.List (elemIndex, maximumBy)
import Data.Ord (comparing)

main :: IO ()
main = print $ fst $ maximumBy (comparing snd) periods

limit :: Int
limit = 1_000 - 1

periods :: [(Int, Int)]
periods = map (\x -> (x, period 1 x [])) [1 .. limit]

period :: Int -> Int -> [Int] -> Int
period num denom visited =
  if remainder == 0
    then 0
    else case elemIndex remainder visited of
      Nothing -> period remainder denom (remainder : visited)
      Just idx -> idx + 1
  where
    remainder = (10 * num) `mod` denom
