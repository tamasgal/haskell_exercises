module Golf where

import Data.List

skips :: [a] -> [[a]]
skips a = map (\n -> every n a) [1..(length a)]
  where every n xs = case drop (n - 1) xs of
                     []     -> []
                     (y:ys) -> y : every n ys


localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs) = (if b > a && b > c then [b] else []) ++ localMaxima (c:xs)
localMaxima _ = []


histogram :: [Integer] -> String
histogram nums = map (\n -> count n nums) [0..9]
    where count n (x:xs) = 'n'
        -- | n == x     = 1 + count n xs
        -- | otherwise  = count n xs
