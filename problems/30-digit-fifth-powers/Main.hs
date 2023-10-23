module Main where

-- when adding `n` power of fives together, the resulting sum may only have `n`
-- digits (i.e. needs to be less than 10^n). the largest sum we can build is by
-- using 9 as the base, which results in the sum `9^5 + 9^5 + ... + 9^5` or
-- `n * 9^5`. therefore, we can compute the max value of `n` using the following
-- formula: `10^(n-1) < n * 9^5 < 10^n`, which works out to result in a max `n`
-- of `6` and results in the max number of `6 * 9^5 = 354'294`

main :: IO ()
main = print $ sum nums

power :: Int
power = 5

limit :: Int
limit = 6

nums :: [Int]
nums = filter isValidPowerSum [10 .. 354294]

isValidPowerSum :: Int -> Bool
isValidPowerSum n = n == buildPowerSum n

buildPowerSum :: Int -> Int
buildPowerSum 0 = 0
buildPowerSum n = powers !! (n `mod` 10) + buildPowerSum (n `div` 10)

powers :: [Int]
powers = [x ^ power | x <- [0 .. 9]]
