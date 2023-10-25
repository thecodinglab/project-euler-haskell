module Main where

import Data.Foldable (find)
import Data.Maybe (mapMaybe)
import ProjectEuler.Prime (isPrime, primes)

main :: IO ()
main = do
  print $ sum $ head $ mapMaybe (extendPrimeSet 3) startingPrimePairs

ceil :: Int
ceil = 10000

ceilPrimes :: [Int]
ceilPrimes = takeWhile (< ceil) primes

startingPrimePairs :: [[Int]]
startingPrimePairs =
  map (\(a, b) -> [a, b]) $
    filter
      (uncurry isPrimePair)
      [(ceilPrimes !! p, ceilPrimes !! q) | p <- [1 .. m], q <- [p .. m]]
  where
    m = length ceilPrimes - 1

extendPrimeSet :: Int -> [Int] -> Maybe [Int]
extendPrimeSet 0 set = Just set
extendPrimeSet i set = do
  next <- findNextPrimeForSet set
  extendPrimeSet (pred i) (next : set)

findNextPrimeForSet :: [Int] -> Maybe Int
findNextPrimeForSet set = find (\x -> notElem x set && isPrimePairSet (x : set)) ceilPrimes

isPrimePairSet :: [Int] -> Bool
isPrimePairSet [] = True
isPrimePairSet [_] = True
isPrimePairSet (x : xs) = all (isPrimePair x) xs && isPrimePairSet xs

isPrimePair :: Int -> Int -> Bool
isPrimePair a b =
  isPrime (concatenate a b) && isPrime (concatenate b a)

concatenate :: Int -> Int -> Int
concatenate a b = read $ show a ++ show b
