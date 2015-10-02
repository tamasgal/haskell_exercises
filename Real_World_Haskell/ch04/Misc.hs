import Data.Char

capCount = filter (isUpper . head) . words
