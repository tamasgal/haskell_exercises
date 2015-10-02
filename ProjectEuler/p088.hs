digits :: Int -> [Int]
digits 0 = []
digits x = (x `mod` 10):(digits (x `div` 10))

digitSet :: Int -> [[Int]]
digitSet k = filter (\x -> not $ 0 `elem` x) $ map digits [n..m]
             where n = 10 ^ (k - 1)
                   m = (10 ^ k) - 1


