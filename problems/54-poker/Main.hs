module Main where

import Data.List (find, sortBy, (\\))
import Data.Map (fromListWith, toList)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  content <- readFile "problems/54-poker/poker.txt"

  let games = map (splitAt 5 . (\line -> map read (words line) :: [Card])) (lines content)
  print $ length $ filter (== GT) $ map winner games

winner :: ([Card], [Card]) -> Ordering
winner (a, b) =
  compare handA handB
  where
    handA = classify $ sort a
    handB = classify $ sort b

classify :: [Card] -> Hand
classify cards =
  head $
    catMaybes
      [ matchRoyalFlush cards,
        matchStraightFlush cards,
        matchFourOfAKind cards,
        matchFullHouse cards,
        matchFlush cards,
        matchStraight cards,
        matchThreeOfAKind cards,
        matchTwoPairs cards,
        matchOnePair cards,
        matchHighestCard cards
      ]

matchHighestCard :: [Card] -> Maybe Hand
matchHighestCard cards = Just $ Hand HighestCard [m] (v \\ [m])
  where
    v = valuesOf cards
    m = maximum v

matchOnePair :: [Card] -> Maybe Hand
matchOnePair cards = do
  kind <- matchNOfKind 2 v
  let hand = [kind, kind]
  Just $ Hand OnePair hand (v \\ hand)
  where
    v = valuesOf cards

matchTwoPairs :: [Card] -> Maybe Hand
matchTwoPairs cards = do
  first <- matchNOfKind 2 v
  second <- matchNOfKind 2 (filter (/= first) v)
  let cond x = x == first || x == second
      hand = filter cond v
  Just $ Hand TwoPairs hand (v \\ hand)
  where
    v = valuesOf cards

matchThreeOfAKind :: [Card] -> Maybe Hand
matchThreeOfAKind cards = do
  kind <- matchNOfKind 3 v
  let hand = [kind, kind, kind]
  Just $ Hand ThreeOfAKind hand (v \\ hand)
  where
    v = valuesOf cards

matchStraight :: [Card] -> Maybe Hand
matchStraight cards
  | isStraight v =
      Just $ Hand Straight v []
  where
    v = valuesOf cards
matchStraight _ = Nothing

matchFlush :: [Card] -> Maybe Hand
matchFlush cards
  | isFlush (suitsOf cards) =
      Just $ Hand Flush (valuesOf cards) []
matchFlush _ = Nothing

matchFullHouse :: [Card] -> Maybe Hand
matchFullHouse cards = do
  triplet <- matchNOfKind 3 v
  pair <- matchNOfKind 2 v
  Just $ Hand FullHouse [triplet, triplet, triplet, pair, pair] []
  where
    v = valuesOf cards

matchFourOfAKind :: [Card] -> Maybe Hand
matchFourOfAKind cards = do
  kind <- matchNOfKind 4 v
  let hand = [kind, kind, kind, kind]
  Just $ Hand FourOfAKind hand (v \\ hand)
  where
    v = valuesOf cards

matchStraightFlush :: [Card] -> Maybe Hand
matchStraightFlush cards
  | isFlush (suitsOf cards) && isStraight (valuesOf cards) =
      Just $ Hand StraightFlush (valuesOf cards) []
matchStraightFlush _ = Nothing

matchRoyalFlush :: [Card] -> Maybe Hand
matchRoyalFlush cards
  | isFlush (suitsOf cards) && valuesOf cards == [Ace, King, Queen, Jack, Ten] =
      Just $ Hand RoyalFlush (valuesOf cards) []
matchRoyalFlush _ = Nothing

suitsOf :: [Card] -> [Suit]
suitsOf = map (\(Card suit _) -> suit)

valuesOf :: [Card] -> [Value]
valuesOf = map (\(Card _ value) -> value)

sort :: (Ord a) => [a] -> [a]
sort = sortBy (flip compare)

matchNOfKind :: Int -> [Value] -> Maybe Value
matchNOfKind n v =
  case find (\(_, count) -> count == n) (frequency v) of
    Nothing -> Nothing
    Just (x, _) -> Just x

isStraight :: [Value] -> Bool
isStraight [] = True
isStraight [_] = True
isStraight (x : y : zs) = x /= Two && y == pred x && isStraight (y : zs)

isFlush :: [Suit] -> Bool
isFlush [] = False
isFlush (x : xs) = all (== x) xs

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency list = toList $ fromListWith (+) [(x, 1) | x <- list]

data Hand = Hand Category [Value] [Value] deriving (Eq, Ord, Show)

data Category
  = HighestCard
  | OnePair
  | TwoPairs
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  | RoyalFlush
  deriving (Eq, Ord, Show)

data Card = Card Suit Value deriving (Show)

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Show)

data Value
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Enum, Show)

values :: [Value]
values = [Two ..]

instance Eq Card where
  (Card _ a) == (Card _ b) = a == b

instance Ord Card where
  compare (Card _ a) (Card _ b) = compare a b

instance Read Card where
  readsPrec _ (valueStr : colorStr : rest) = do
    let parsedValue = case valueStr of
          'A' -> Ace
          'K' -> King
          'Q' -> Queen
          'J' -> Jack
          'T' -> Ten
          '9' -> Nine
          '8' -> Eight
          '7' -> Seven
          '6' -> Six
          '5' -> Five
          '4' -> Four
          '3' -> Three
          '2' -> Two
          _ -> error "Invalid card value"

        parsedColor = case colorStr of
          'H' -> Hearts
          'D' -> Diamonds
          'C' -> Clubs
          'S' -> Spades
          _ -> error "Invalid card color"

    return (Card parsedColor parsedValue, rest)
  readsPrec _ _ = error "Invalid card format"
