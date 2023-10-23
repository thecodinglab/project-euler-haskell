module Main where

main :: IO ()
main = print $ possibilities allCoins 200

allCoins :: [Int]
allCoins = [1, 2, 5, 10, 20, 50, 100, 200]

possibilities :: [Int] -> Int -> Int
possibilities _ 0 = 1
possibilities coins remaining =
  sum
    $ map
      ( \coin ->
          possibilities
            (filter (<= coin) coins) -- filter out coins that could have been used before
            (remaining - coin)
      )
    $ filter (<= remaining) coins
