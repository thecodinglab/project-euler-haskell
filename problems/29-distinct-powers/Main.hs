module Main where

import Data.List (nub)

main :: IO ()
main = print $ length distinctPowers

limit :: Integer
limit = 100

distinctPowers :: [Integer]
distinctPowers = nub [a ^ b | a <- [2 .. limit], b <- [2 .. limit]]
