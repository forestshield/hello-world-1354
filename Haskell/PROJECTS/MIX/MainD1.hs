import Control.Exception (catch, SomeException)
import System.Environment (getArgs)
import System.Directory (doesFileExist)

-- Counts the number of words per line in a file
-- Does not crash if file is missing.

main :: IO ()
main = do
    -- variant #1
{-    
    input <- readFile "input.txt"
-}
    -- variant #2    
    args <- getArgs
    let fileName = case args of
            (a:_) -> a
            _ -> "input.txt"
            --_ -> "GTK.hs"
        
    exists <- doesFileExist fileName
    input2 <- if exists then readFile fileName else return ""
  
    input <- catch (readFile fileName)
            $ \err -> print (err::SomeException) >> return ""
    
    print $ countWords input

    -------------
    --putStrLn "Enter your number:"
    --num <- getLine  
    --doGuessing num

-----------
-- online indent sample
doGuessing num2 = do
  --putStrLn "Enter your number:"
  --num <- getLine  
  putStrLn "Enter your guess:"
  guess <- getLine
  case compare (read guess) num2 of
    LT -> do putStrLn "Too low!"
             doGuessing num2
    GT -> do putStrLn "Too high!"
             doGuessing num2
    EQ -> putStrLn "You Win!"

countWords :: String -> [Int]
countWords input = map (length.words) (lines input)

