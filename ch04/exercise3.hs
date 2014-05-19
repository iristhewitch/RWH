-- file: ch04/exercise3.hs

import System.Environment (getArgs)

--printFirstWord input =  unlines (map head (map words (lines input)))


interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"

          -- replace "id" with the name of our function below
          myFunction = printFirstWord