module Main where

main :: IO ()
main = print $ sum $ filter even $ takeWhile (< 4_000_000) fibs

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
