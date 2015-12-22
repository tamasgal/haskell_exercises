myLast :: [a] -> a
myLast [] = error "No last element in empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' = foldr1 (const id)
