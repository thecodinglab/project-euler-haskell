module Main where

main :: IO ()
main = print $ maximum $ primeFactors target

primeFactors :: Integer -> [Integer]
primeFactors 0 = []
primeFactors n = case factors of
  [] -> [n]
  list -> list ++ primeFactors (n `div` head list)
  where
    factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. (n - 1)]

target :: Integer
target = 600851475143
