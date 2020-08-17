
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Control.Exception.Base


main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  

handler :: IOError -> IO ()
handler e = putStrLn "Whoops, had some trouble!"


{-
import System.Environment  
import System.IO  
import System.IO.Error  
  
main = do
  --toTry `catch` handler  
              
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise = ioError e 
-}

{-
-- this one works 
import Control.Exception
import Control.Exception.Base
import Data.Array

main = toTry `catch` handler

toTry = do
    print "hi"
    print (show (3 `div` 0))
    print "hi"

handler :: ArithException -> IO ()
handler DivideByZero = putStrLn "Divide by Zero!"
handler _ = putStrLn "Some other error..."
-}
















