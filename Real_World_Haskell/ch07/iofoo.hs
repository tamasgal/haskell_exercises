import Data.Char(toUpper)

foo :: String -> IO ()
foo s = putStrLn (map toUpper s)

main = foo "test"

