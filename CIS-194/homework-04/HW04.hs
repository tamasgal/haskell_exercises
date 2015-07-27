xor :: [Bool] -> Bool
xor = foldr ((==) . const False) False . filter (== True)

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1)
      . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

map' :: (a -> b) -> [a] -> [b]
-- map' f = foldr (\x y -> (f x):y) []
-- (\x y -> f x : y) can be written like this: (\x -> (:) (f x)).
-- which we can further reduce to: ((:) . f)
map' f = foldr ((:) . f) []


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: Eq a => [a] -> Tree a
foldTree xs = foldr (insert first) Leaf xs
    where first = floor (logBase 2 $ fromIntegral(length xs)::Double)

insert :: Int -> a -> Tree a -> Tree a
insert _ _ _ = Leaf
