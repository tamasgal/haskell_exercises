xor :: [Bool] -> Bool
xor = foldr (==) False . map (\n -> False) . filter (==True)
