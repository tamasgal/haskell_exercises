{-# OPTIONS_GHC -Wall #-}
module HW02 where
--import Data.List
-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = length $ filter (\x -> fst x == snd x) $ zip c1 c2

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (count code) colors
  where count c p = length $ filter (\x -> x == p) c

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = sum $ zipWith min (countColors c1) (countColors c2)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonExact
  where exact    = exactMatches secret guess
        nonExact = (matches secret guess) - exact


-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move c1 e m) c2
    | (e, m) == getMatches (getMove c2 c1) = True
    | otherwise = False
    where getMatches (Move _ e_ m_) = (e_, m_)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes _ [] = []
filterCodes m (c:cs) = if isConsistent m c then c:(filterCodes m cs) else (filterCodes m cs)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
  | n == 0 = []
  | n == 1 = [colors]
  | n >= 2 = [colors]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
