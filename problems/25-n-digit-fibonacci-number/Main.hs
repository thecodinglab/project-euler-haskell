module Main where

import Data.List (findIndex)

main :: IO ()
main = print $ findIndex (>= smallest) fibs

digits :: Integer
digits = 1000

smallest :: Integer
smallest = 10 ^ (digits - 1)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
