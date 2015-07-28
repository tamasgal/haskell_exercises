-- Solutions for Homework #1 from the CIS 194 - Haskell course
-- http://www.seas.upenn.edu/%7Ecis194/spring13/lectures
--
-- Description:
-- http://www.seas.upenn.edu/%7Ecis194/spring13/hw/01-intro.pdf
--
-- Author: Tamas Gal


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c
  | x == 1 = [(a, c)]
  | otherwise  = hanoi (x-1) a c b ++ hanoi 1 a b c ++ hanoi (x-1) b a c
