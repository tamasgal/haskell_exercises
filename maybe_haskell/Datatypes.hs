data Person = PersonWithAge String Int | PersonWithoutAge String

getName :: Person -> String
getName (PersonWithAge name _) = name
getName (PersonWithoutAge name) = name

getAge :: Person -> Maybe Int
getAge (PersonWithAge name age) = Just age
getAge (PersonWithoutAge name) = Nothing


find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find predicate (first:rest) =
    if predicate first
        then Just first
        else find predicate rest
