module Main where

main :: IO ()
main = print $ sum $ filter multipleOfThreeOrFive [1 .. 999]

multipleOfThreeOrFive :: Integer -> Bool
multipleOfThreeOrFive v = (v `mod` 3 == 0) || (v `mod` 5 == 0)
