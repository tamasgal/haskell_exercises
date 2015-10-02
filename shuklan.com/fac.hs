-- Solutions for the "Think Differently" excercise at shuklan.com
-- http://shuklan.com/haskell
-- 
-- Description:
-- http://shuklan.com/haskell/lec02.html#/0/22
-- 
-- Author: Tamas Gal


facA :: Integer -> Integer
facA n
  | n == 0     = 1
  | otherwise = n * facA (n-1)

facB :: Integer -> Integer
facB n = product [1..n]

facC :: Integer -> Integer
facC n = foldl (*) 1 [1..n]

facD :: Integer -> Integer
facD n = if n < 2 then 1 else n * facD (n-1)
