dividible :: [Int] -> Int -> Bool
dividible _ 0      = False
dividible [] _     = True
dividible (x:xs) n = dividibleBy n x && (dividible xs n)
  where dividibleBy n x = n `mod` x == 0

answer = head $ filter (dividible [1..20]) [0,20..]

