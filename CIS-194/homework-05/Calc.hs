{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Parser
import Data.Maybe

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul
