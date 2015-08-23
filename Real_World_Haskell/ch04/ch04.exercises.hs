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
