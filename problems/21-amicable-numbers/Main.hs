module Main where

import Data.List (nub)

main :: IO ()
main = print $ sum amicable

limit :: Int
limit = 10_000 - 1

amicable :: [Int]
amicable = [n | n <- [1 .. limit], isAmicable n]

isAmicable :: Int -> Bool
isAmicable n = isAmicablePair n (sum (properDivisors n))

isAmicablePair :: Int -> Int -> Bool
isAmicablePair a b =
  a /= b && sum (properDivisors a) == b && sum (properDivisors b) == a

properDivisors :: Int -> [Int]
properDivisors n = filter (< n) (divisors n)

divisors :: Int -> [Int]
divisors n = nub $ concat [[x, n `div` x] | x <- [1 .. lim], n `mod` x == 0]
  where
    lim = floor $ sqrt (fromIntegral n :: Double) :: Int
