divisors n = filter (\x -> n `mod` x == 0) [1..n-1]
abundant x = (sum $ divisors x) > x
maxSum = sum [1..28123]
abundants = filter abundant [1..28123]
abundantCombs = filter (<=28123) $ zipWith (+) abundants abundants
answer = maxSum - (sum abundantCombs)
