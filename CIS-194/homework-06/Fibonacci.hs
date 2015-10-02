{-# OPTIONS_GHC -Wall #-}

fib :: Integer -> Integer
fib n
 | n == 0 = 0
 | n == 1 = 1
 | otherwise = fib (n -1) + fib (n - 2)


fibs1 :: [Integer]
fibs1 = [fib(n) | n <- [0..]] -- or fmap fib [0..]

fibs2 :: [Integer]
fibs2 = map fst (iterate f (0,1)) where f (x,y) = (y,x+y)
