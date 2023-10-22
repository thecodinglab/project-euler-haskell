module Main where

import Data.IntMap qualified as Map
import Data.List (nub)

main :: IO ()
main =
  print $
    multiplyPrimeFactors $
      combinePrimeFactors $
        map (countPrimeFactors . primeFactors) [1 .. 20]

multiplyPrimeFactors :: Map.IntMap Int -> Int
multiplyPrimeFactors = Map.foldrWithKey (\x count acc -> acc * x ^ count) 1

combinePrimeFactors :: [Map.IntMap Int] -> Map.IntMap Int
combinePrimeFactors = foldr accumulatePrimeFactors Map.empty

accumulatePrimeFactors :: Map.IntMap Int -> Map.IntMap Int -> Map.IntMap Int
accumulatePrimeFactors factors res =
  Map.foldrWithKey (Map.insertWith max) res factors

countPrimeFactors :: [Int] -> Map.IntMap Int
countPrimeFactors x =
  Map.fromList $ map (\val -> (val, length $ filter (== val) x)) $ nub x

primeFactors :: Int -> [Int]
primeFactors 0 = []
primeFactors n = case factors of
  [] -> [n]
  list -> list ++ primeFactors (n `div` head list)
  where
    factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. (n - 1)]
