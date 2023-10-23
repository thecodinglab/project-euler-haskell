module Main where

import Data.List (maximumBy)
import Data.Ord (comparing)

main :: IO ()
main =
  putStrLn $
    "n^2 "
      ++ (if a < 0 then "-" else "+")
      ++ " "
      ++ show (abs a)
      ++ "n + "
      ++ show b
      ++ " -> "
      ++ show numPrimes
      ++ "\na * b = "
      ++ show prod
  where
    (a, b, numPrimes) = maxNumberOfPrimes
    prod = a * b

maxNumberOfPrimes :: (Int, Int, Int)
maxNumberOfPrimes = maximumBy (comparing trd) primeLengths

trd :: (a, b, c) -> c
trd (_, _, v) = v

primeLengths :: [(Int, Int, Int)]
primeLengths =
  [ (a, b, numberOfPrimes a b)
    | a <- [(-999) .. 999],
      b <- [(-1000) .. 1000]
  ]

numberOfPrimes :: Int -> Int -> Int
numberOfPrimes a b = length $ takeWhile isPrime $ series a b

series :: Int -> Int -> [Int]
series a b = [n * n + a * n + b | n <- [0 ..]]

-- Sive of Eratosthenes
-- Source: https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
primes :: [Int]
primes = 2 : [x | x <- [3 ..], isPrime x]

isPrime :: Int -> Bool
isPrime x = all (\p -> x `mod` p > 0) (factorsToTry $ abs x)
  where
    factorsToTry n = takeWhile (\p -> p * p <= n) primes
