import qualified Data.Set as S (Set, empty, fromList, insert)

sets :: [Integer] -> [Integer] -> S.Set Integer
sets ps = S.fromList . concatMap (g ps) where
  g [] _ = []
  g (x:xs) y = (x ^ y) : g xs y

main :: IO ()
main = print . length $ sets [2..100] [2..100]
