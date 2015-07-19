answer = sum [1..100] ^ 2 - foldl (\x y -> y^2 + x) 0 [1..100]
