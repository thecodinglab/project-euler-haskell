module Main where

main :: IO ()
main = print $ simplify $ multiplyAll $ case2 ++ case3

digits :: [Int]
digits = [1 .. 9]

type Frac = (Int, Int)

multiplyAll :: [Frac] -> Frac
multiplyAll = foldr multiply (1, 1)

multiply :: Frac -> Frac -> Frac
multiply (anum, adenom) (bnum, bdenom) = (anum * bnum, adenom * bdenom)

simplify :: Frac -> Frac
simplify (num, denom) = (num `div` g, denom `div` g)
  where
    g = gcd num denom

-- Case 1: (10a + b) / (10a + c) = b / c  where b < c
--         10ac + bc = 10ab + bc
--         10ac = 10ab
--         c = b
-- => contradicts b < c

-- Case 2: (10a + b) / (10c + a) = b / c  where b < c
--         10ac + bc = 10bc + ab
--         10ac = 9bc + ab
case2 :: [Frac]
case2 =
  map (\(a, b, c) -> (10 * a + b, 10 * c + a)) $
    filter
      (\(a, b, c) -> 10 * a * c == 9 * b * c + a * b)
      [(a, b, c) | a <- digits, b <- digits, c <- digits, b < c]

--
-- Case 3: (10a + b) / (10b + c) = a / c  where a < c
--         10ac + bc = 10ab + ac
--         9ac + bc = 10ab
case3 :: [Frac]
case3 =
  map (\(a, b, c) -> (10 * a + b, 10 * b + c)) $
    filter
      (\(a, b, c) -> 9 * a * c + b * c == 10 * a * b)
      [(a, b, c) | a <- digits, b <- digits, c <- digits, a < c]

-- Case 4: (10a + b) / (10c + b) = a / c  where a < c
--         10ac + bc = 10ac + ab
--         bc = ab
--         c = a
-- => contradicts a < c
