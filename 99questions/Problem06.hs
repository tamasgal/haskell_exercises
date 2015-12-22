isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = all (uncurry (==)) $ zip xs $ reverse xs

