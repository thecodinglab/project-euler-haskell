module Main where

import Data.List (nub)
import Math.NumberTheory.Roots (integerSquareRoot)

-- divisors are symmetrical and therefore only need to be searched for until
-- sqrt(x)
--
-- example: 28 -> [1, 2, 4, 7, 14, 28] (sqrt 28 approx 5.3)
--            28 / 1 = 28
--            28 / 2 = 14
--            28 / 4 = 7

main :: IO ()
main = print $ head $ filter (\x -> length (divisors x) > 500) triangular

divisors :: Int -> [Int]
divisors n = nub $ concat [[x, n `div` x] | x <- [1 .. lim], n `mod` x == 0]
  where
    lim = integerSquareRoot n

triangular :: [Int]
triangular = map (\n -> (n * (n + 1)) `div` 2) [1 ..]
