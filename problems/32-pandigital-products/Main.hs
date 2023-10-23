module Main where

import Data.List (nub, permutations)

main :: IO ()
-- main = print pandigital
main = print $ sum pandigital

pandigital :: [Int]
pandigital =
  nub
    $ map (\(_, _, c) -> c)
    $ filter (\(a, b, c) -> a * b == c)
    $ concatMap
      ( \x ->
          [ (read $ take a x, read $ take b $ drop a x, read $ drop (a + b) x)
            | a <- [1 .. 9 - 2],
              b <- [1 .. 9 - a - 1]
          ]
      )
    $ permutations "123456789"
