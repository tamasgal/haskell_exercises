-- file: ch04/InteractWith.hs

import System.Environment (getArgs)

interactWith function inputFile = do
    input <- readFile inputFile
    putStrLn (function input)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [input] -> interactWith function input
                _ -> putStrLn "error: exactly one argument needed"
          -- replace "id" with the name of our function below
          myFunction = firstWords 

firstWords :: String -> String
firstWords input = head $ lines input
