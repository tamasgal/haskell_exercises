import Data.List
length $ nub $ sort [x ^ y | x <- [2..100], y <- [2..100]]

