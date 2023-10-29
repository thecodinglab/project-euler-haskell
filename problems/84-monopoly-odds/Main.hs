module Main where

import Data.List (intercalate, sortBy)
import System.Random (RandomGen, mkStdGen, uniformR)

main :: IO ()
main =
  let gen = mkStdGen 1234
      game =
        Game
          { position = GO,
            communityChest = initialCommunityChest,
            chance = initialChance,
            consecutiveDoubles = 0,
            visits = replicate 40 0
          }
      (end, _) =
        foldr
          (\_ (a, b) -> roll a b)
          (game, gen)
          ([1 .. 10_000_000] :: [Int])
   in do
        putStrLn $
          mkSolutionString $
            map snd $
              take 3 $
                sortBy (\(a, _) (b, _) -> compare b a) $
                  zip (visits end) (map toEnum [0 ..] :: [Position])

--------------------------
-- Game                 --
--------------------------

diceSides :: Int
diceSides = 4

data Game = Game
  { position :: Position,
    communityChest :: Stack CommunityChest,
    chance :: Stack Chance,
    consecutiveDoubles :: Int,
    visits :: [Int]
  }
  deriving (Show)

roll :: (RandomGen g) => Game -> g -> (Game, g)
roll game gen =
  if consecutive == 3
    then (visit (game {consecutiveDoubles = 0}) JAIL, diceBGen)
    else (visit (game {consecutiveDoubles = consecutive}) nextPos, diceBGen)
  where
    (diceA :: Int, diceAGen) = uniformR (1, diceSides) gen
    (diceB :: Int, diceBGen) = uniformR (1, diceSides) diceAGen
    diceRoll = diceA + diceB
    nextPos = advance diceRoll $ position game

    double = diceA == diceB
    consecutive = if double then consecutiveDoubles game + 1 else 0

visit :: Game -> Position -> Game
visit game pos =
  next {visits = visited}
  where
    next = visit' game pos
    idx = fromEnum (position next)
    visited = replaceAt (succ (visits game !! idx)) idx (visits game)

visit' :: Game -> Position -> Game
visit' game pos
  | isCommunityChest pos =
      let (card, stack) = draw $ communityChest game
       in game {position = applyCommunityChest pos card, communityChest = stack}
  | isChance pos =
      let (card, stack) = draw $ chance game
       in game {position = applyChance pos card, chance = stack}
  | pos == G2J = game {position = JAIL}
  | otherwise = game {position = pos}

--------------------------
-- Cards                --
--------------------------

type Stack a = [a]

draw :: Stack a -> (a, Stack a)
draw [] = error "unable to draw from empty card stack"
draw (x : xs) = (x, xs ++ [x])

data CommunityChest = CCGo | CCJail | CCNothing
  deriving (Show)

initialCommunityChest :: Stack CommunityChest
initialCommunityChest =
  [ CCGo,
    CCJail,
    CCNothing,
    CCNothing,
    CCNothing,
    CCNothing,
    CCNothing,
    CCNothing,
    CCNothing,
    CCNothing,
    CCNothing,
    CCNothing,
    CCNothing,
    CCNothing,
    CCNothing,
    CCNothing
  ]

isCommunityChest :: Position -> Bool
isCommunityChest pos = pos == CC1 || pos == CC2 || pos == CC3

applyCommunityChest :: Position -> CommunityChest -> Position
applyCommunityChest _ CCGo = GO
applyCommunityChest _ CCJail = JAIL
applyCommunityChest curr CCNothing = curr

data Chance = CHGo | CHJail | CHC1 | CHE3 | CHH2 | CHR1 | CHNextR | CHNextU | CHPrev3 | CHNothing
  deriving (Show)

initialChance :: Stack Chance
initialChance =
  [ CHGo,
    CHJail,
    CHC1,
    CHE3,
    CHH2,
    CHR1,
    CHNextR,
    CHNextR,
    CHNextU,
    CHPrev3,
    CHNothing,
    CHNothing,
    CHNothing,
    CHNothing,
    CHNothing,
    CHNothing
  ]

isChance :: Position -> Bool
isChance pos = pos == CH1 || pos == CH2 || pos == CH3

applyChance :: Position -> Chance -> Position
applyChance _ CHGo = GO
applyChance _ CHJail = JAIL
applyChance _ CHC1 = C1
applyChance _ CHE3 = E3
applyChance _ CHH2 = H2
applyChance _ CHR1 = R1
applyChance CH1 CHNextR = R2
applyChance CH2 CHNextR = R3
applyChance CH3 CHNextR = R1
applyChance curr CHNextR = curr
applyChance CH1 CHNextU = U1
applyChance CH2 CHNextU = U2
applyChance CH3 CHNextU = U1
applyChance curr CHNextU = curr
applyChance curr CHPrev3 = advance (-3) curr
applyChance curr CHNothing = curr

--------------------------
-- Board                --
--------------------------

data Position = GO | A1 | CC1 | A2 | T1 | R1 | B1 | CH1 | B2 | B3 | JAIL | C1 | U1 | C2 | C3 | R2 | D1 | CC2 | D2 | D3 | FP | E1 | CH2 | E2 | E3 | R3 | F1 | F2 | U2 | F3 | G2J | G1 | G2 | CC3 | G3 | R4 | CH3 | H1 | T2 | H2
  deriving (Eq, Ord, Enum, Show)

advance :: Int -> Position -> Position
advance n pos
  | next < 0 = advance (next + 1) H2
  | next > 39 = advance (next - 40) GO
  | otherwise = toEnum next
  where
    next = fromEnum pos + (n `mod` 40)

mkSolutionString :: [Position] -> String
mkSolutionString pos = intercalate "" $ map mkSolutionPart pos

mkSolutionPart :: Position -> String
mkSolutionPart pos =
  replicate (2 - length str) '0' ++ str
  where
    str = show $ fromEnum pos

--------------------------
-- Other                --
--------------------------

replaceAt :: a -> Int -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt r 0 (_ : xs) = r : xs
replaceAt r i (x : xs)
  | i > 0 = x : replaceAt r (pred i) xs
  | otherwise = error "negative index"
