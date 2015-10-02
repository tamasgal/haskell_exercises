combs = [x * 10 + y | x <- [1..9] , y <- [1..9]]
candidates = [(x,y) | x <- combs, y <- combs, x `mod` y == 0

