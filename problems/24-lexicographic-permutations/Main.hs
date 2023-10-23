module Main where

import Data.List (permutations, sort)

main :: IO ()
main = print $ perm !! (1_000_000 - 1)

perm :: [String]
perm = sort $ permutations "0123456789"
