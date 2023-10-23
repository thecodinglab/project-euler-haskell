module Main where

import Control.Monad (liftM2)
import Data.List (nub, sort)

main :: IO ()
main = print $ sum $ findNonAbundantSums [1 .. limit] sumOfTwoAbundant []

limit :: Int
limit = 28123

abundant :: [Int]
abundant = [x | x <- [1 .. limit], sum (properDivisors x) > x]

sumOfTwoAbundant :: [Int]
sumOfTwoAbundant =
  sort $ filter (<= limit) $ map (uncurry (+)) $ liftM2 (,) abundant abundant

findNonAbundantSums :: [Int] -> [Int] -> [Int] -> [Int]
findNonAbundantSums [] _ res = res
findNonAbundantSums i [] res = res ++ i
findNonAbundantSums i@(ih : it) a@(ah : at) res
  | ih == ah = findNonAbundantSums it at res
  | ih < ah = findNonAbundantSums it a (res ++ [ih])
  | otherwise = findNonAbundantSums i at res

properDivisors :: Int -> [Int]
properDivisors n = filter (< n) (divisors n)

divisors :: Int -> [Int]
divisors n = nub $ concat [[x, n `div` x] | x <- [1 .. lim], n `mod` x == 0]
  where
    lim = floor $ sqrt (fromIntegral n :: Double) :: Int
