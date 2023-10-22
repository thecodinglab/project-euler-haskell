module Main where

-- a, b, c in N
--
-- a < b < c
-- a^2 + b^2 = c^2
-- a + b + c = 1000
-- => c = 1000 - b - a

main :: IO ()
main =
  putStrLn (show solutionTriplet ++ ": " ++ show solution)
  where
    solutionTriplet =
      head $
        filter (\(a, b, c) -> a ^ (2 :: Int) + b ^ (2 :: Int) == c ^ (2 :: Int)) $
          filter (\(a, b, c) -> a < b && b < c) triplets

    (sa, sb, sc) = solutionTriplet
    solution = sa * sb * sc

maxVal :: Int
maxVal = 1000

triplets :: [(Int, Int, Int)]
triplets = [(x, y, 1000 - x - y) | x <- [1 .. maxVal], y <- [1 .. maxVal]]
