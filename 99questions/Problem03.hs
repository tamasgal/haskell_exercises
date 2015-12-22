elementAt :: [a] -> Int -> a
elementAt xs i = last $ take i xs
