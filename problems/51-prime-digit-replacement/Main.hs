module Main where

import Data.Foldable (maximumBy)
import Data.List (subsequences)
import ProjectEuler.Prime (isPrime, primes)

main :: IO ()
main =
  print $
    minimum $
      head $
        filter (\x -> length x == 8) $
          map findMaxPrimes primes

findMaxPrimes :: Int -> [Int]
findMaxPrimes prime =
  maximumBy (\a b -> compare (length a) (length b)) $ findPrimesAfterReplacing prime

findPrimesAfterReplacing :: Int -> [[Int]]
findPrimesAfterReplacing prime =
  map (filterPrimesFor primeStr) indices
  where
    primeStr = show prime
    indices = tail $ subsequences [0 .. length primeStr - 1]

filterPrimesFor :: String -> [Int] -> [Int]
filterPrimesFor str indices =
  filter
    isPrime
    [read (replaceAtAll d indices str) | d <- digitsForIndices indices]

digitsForIndices :: [Int] -> [Char]
digitsForIndices lst
  | 0 `elem` lst = ['1' .. '9']
  | otherwise = ['0' .. '9']

replaceAtAll :: a -> [Int] -> [a] -> [a]
replaceAtAll r i l = foldr (replaceAt r) l i

replaceAt :: a -> Int -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt r 0 (_ : xs) = r : xs
replaceAt r i (x : xs)
  | i > 0 = x : replaceAt r (pred i) xs
  | otherwise = error "negative index"
