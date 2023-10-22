module Main where

main :: IO ()
main = print $ sumOfNaturalNumbersUtil n ^ 2 - sumOfSquareNaturalNumbersUntil n
  where
    n = 100

sumOfSquareNaturalNumbersUntil :: Int -> Int
sumOfSquareNaturalNumbersUntil n = (n * (n + 1) * (2 * n + 1)) `div` 6

sumOfNaturalNumbersUtil :: Int -> Int
sumOfNaturalNumbersUtil n = (n * (n + 1)) `div` 2
