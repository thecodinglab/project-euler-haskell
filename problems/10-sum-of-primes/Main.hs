module Main where

main :: IO ()
main = print $ sum $ takeWhile (< 2_000_000) primes

-- Sive of Eratosthenes
-- Source: https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
primes :: [Int]
primes = 2 : [x | x <- [3 ..], isPrime x]

isPrime :: Int -> Bool
isPrime x = all (\p -> x `mod` p > 0) (factorsToTry x)
  where
    factorsToTry n = takeWhile (\p -> p * p <= n) primes
