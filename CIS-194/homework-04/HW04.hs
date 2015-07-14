xor :: [Bool] -> Bool
xor = foldr (==) False . map (\n -> False) . filter (==True)


fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x-2) . filter even


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x):y) []
