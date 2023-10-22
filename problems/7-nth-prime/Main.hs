module Main where

main :: IO ()
main = print $ filter isPrime [1 ..] !! 10_001

isPrime :: Int -> Bool
isPrime x = all (\n -> x `mod` n /= 0) [2 .. (x - 1)]
