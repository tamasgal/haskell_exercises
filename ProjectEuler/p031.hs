change :: [Int] -> Int -> Int
change _ 0      = 1
change [x] _    = 1
change (x:xs) s = sum $ map (change xs . (s-)) [0,x..s]

answer = change [200,100,50,20,10,5,2,1] 200
