module Main where

from :: Integer
from = 100

to :: Integer
to = 999

main :: IO ()
main =
  print $
    maximum $
      filter (isPalindrom . show) $
        concatMap (\x -> map (* x) [from .. to]) [from .. to]

isPalindrom :: String -> Bool
isPalindrom x = x == reverse x
