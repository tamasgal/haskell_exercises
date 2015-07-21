change :: [Integer] -> Integer -> Integer
change _ 0      = 1
change [c] _    = 1
change (c:cs) s = sum $ map (change cs . (s-)) [0,c..s]

answer = change [200,100,50,20,10,5,2,1] 200
