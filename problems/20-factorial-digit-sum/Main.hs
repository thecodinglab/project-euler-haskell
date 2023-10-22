module Main where

main :: IO ()
main = print $ sumOfDigits $ factorial 100

sumOfDigits :: Integer -> Integer
sumOfDigits n = sum $ map (\x -> read [x] :: Integer) (show n)

factorial :: Integer -> Integer
factorial n = product [1 .. n]
