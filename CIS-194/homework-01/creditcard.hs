-- Solutions for Homework #1 from the CIS 194 - Haskell course
-- http://www.seas.upenn.edu/%7Ecis194/spring13/lectures
--
-- Description:
-- http://www.seas.upenn.edu/%7Ecis194/spring13/hw/01-intro.pdf
--
-- Author: Tamas Gal


-- Create a list from each digit of an Int
toDigits x = reverse (toDigitsRev x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x 
  | x <= 0    = []
  | otherwise = (x `mod` 10) : toDigitsRev (x `div` 10)


-- Double the value of every second digit
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [2*x]
doubleEveryOther (x:y:zs) = x : 2*y : doubleEveryOther zs


-- Add the digits of an Int
digitSum :: Integer -> Integer
digitSum x
  | x < 10    = x
  | otherwise = (x `mod` 10) + digitSum (x `div` 10)


-- Return the sum of digits for all integers in a list
sumDigits :: [Integer] -> Integer
sumDigits [x] = digitSum x
sumDigits (x:xs) = digitSum x + sumDigits xs


-- Validate credit card number
validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigitsRev x)) `mod` 10 == 0
