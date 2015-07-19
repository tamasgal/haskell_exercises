import Data.List
answer = length $ nub $ [x ^ y | x <- [2..100], y <- [2..100]]

main = do putStrLn (show answer)
