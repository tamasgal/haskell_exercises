import Data.List
import Control.Arrow

data Encoded a = Single a | Multiple Int a
    deriving (Show)

encode :: Eq a => [a] -> [(Int, a)]
encode = map (length &&& head) . group

encodeX :: Eq a => [a] -> [Encoded a]
encodeX = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x
