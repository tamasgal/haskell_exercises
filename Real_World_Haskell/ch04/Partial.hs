import Data.List
isInAny2 needle haystack = any (\s -> needle `isInfixOf` s) haystack

isInAny needle haystack = any inSequence haystack
    where inSequence s = needle `isInfixOf`s
