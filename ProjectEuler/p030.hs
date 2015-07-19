digits :: Int -> [Int]
digits 0 = []
digits x = (x `mod` 10):(digits (x `div` 10))

answer = sum $ filter (\x -> x == (sum $ map (^5) (digits x))) [2..999999]

main = do putStrLn (show answer)
