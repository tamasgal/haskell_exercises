-- Solutions for the "Reverse a List" excercise at shuklan.com
-- http://shuklan.com/haskell
-- 
-- Description:
-- http://shuklan.com/haskell/lec02.html#/0/14
-- 
-- Author: Tamas Gal


rev [] = []
rev xs = last xs : rev (init xs)
