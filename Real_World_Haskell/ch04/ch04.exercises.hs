import Data.Char (digitToInt)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs


safeListFuncWrapper func [] = Nothing
safeListFuncWrapper func xs = Just (func xs)

xSafeHead = safeListFuncWrapper head
xSafeTail = safeListFuncWrapper tail
xSafeLast = safeListFuncWrapper last
xSafeInit = safeListFuncWrapper init

asIntFold :: String -> Maybe Int
asIntFold "" = Nothing
asIntFold "-" = Nothing
asIntFold ('-':xs) = asIntFold xs
asIntFold xs = Just (foldl step 0 xs)
               where step x y = 10 * x + digitToInt y

