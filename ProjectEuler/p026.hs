-- Return a list of digits for a division of two integers
recipDigits :: Int -> Int -> [Int]
-- recipDigits 0 _ = []
recipDigits p q = (p `div` q) : (recipDigits ((p `mod` q) * 10) q)

split :: [a] -> ([a], [a])
split xs = splitAt (((length xs) + 1) `div` 2) xs

repeated :: Int -> [Int] -> [Int]
repeated 0 _ = []
-- repeated 1 (x:y:z:_) = if x == y && y == z then [x] else []
repeated n xs = if subset == next then subset else (repeated (n-1) xs)
              where seq = take (2*n) xs
                    (subset,next) = split seq

checkRepeat :: Int -> [Int] -> [Int]
checkRepeat _ [] = []
checkRepeat _ [x] = []
checkRepeat n seq@(x:xs) = if (repeated n seq) /= []
                             then (repeated n seq)
                             else (repeated n xs)
