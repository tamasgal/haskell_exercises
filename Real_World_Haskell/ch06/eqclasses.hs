class BasicEq a where
    isEqual :: a -> a -> Bool


instance BasicEq Bool where
    isEqual True True = True
    isEqual False False = False
    isEqual _ _ = False

instance BasicEq Int where
    isEqual _ _ = False


class BasicEq2 a where
    isEqual2    :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool



class BasicEq3 a where
    (<>), (<!>) :: a -> a -> Bool
    (<>) x y = not ((<!>) x y)
    (<!>) x y = not ((<>) x y)


instance BasicEq3 Int where
    (<>) x y = x == y


data Color = Red | Green | Blue

instance Show Color where
    show _ = "Some color"



data MyType = MyType (Int -> Bool)

