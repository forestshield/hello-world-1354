
import System.Environment
import Data.List
import Control.Exception
import Control.Exception.Base
import Data.Array

main = toTry `catch` handler
---
toTry = do
  
    args <- getArgs
    progName <- getProgName    
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn progName    

    let nDivisor = (read $ args !! 0) :: Int

    if (length args) > 0
        then
            --print (show (3 `div` (read $ args !! 0) :: Int))            
            --putStr "Divisor is: "
            --putStrLn show nDivisor
            print $ show (3 `div` nDivisor)
        else
            print "No arguments in program"  

--    print "What is a second number?"
--    numStr1 <- getLine 
--    print (show (3 `div` (read numStr1) :: Int))
    --print (show (3 `div` 0))
    --print (show (3 `div` 1))
    --print "hi"
---
handler :: ArithException -> IO ()
handler DivideByZero = putStrLn "Divide by Zero!"
handler _ = putStrLn "Some other error..."

















