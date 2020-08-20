
import Control.Exception
import Control.Exception.Base
import Data.Array

main = toTry `catch` handler
---
toTry = do
    print "What is a second number?"
    numStr1 <- getLine 
    print (show (3 `div` (read numStr1) :: Int))
    --print (show (3 `div` 0))
    --print (show (3 `div` 1))
    print "hi"
---
handler :: ArithException -> IO ()
handler DivideByZero = putStrLn "Divide by Zero!"
handler _ = putStrLn "Some other error..."

















