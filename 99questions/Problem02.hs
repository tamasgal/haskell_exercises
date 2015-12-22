myButLast :: [a] -> a
myButLast [x,_] = x 
myButLast (_:xs) = myButLast xs
myButLast _ = error "Need at least two elements"
