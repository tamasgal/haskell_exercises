fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

sumEven = sum . takeWhile (< 4000000) . filter even $ fibs
