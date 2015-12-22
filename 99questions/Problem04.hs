myLength :: [a] -> Int
myLength = foldr (\ _ x -> 1 + x) 0
