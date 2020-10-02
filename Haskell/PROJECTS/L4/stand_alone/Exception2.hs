import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Control.Exception.Base

--rsFile1 = "/Users/admin1/Haskell/PROJECTS/L4/stand_alone/todo.txt"
--rsFile2 = "~/Haskell/PROJECTS/L4/stand_alone/todo.txt"
rsFile1 = "todo.txt"
rsFile2 = "todo.txt"

{-
main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e
-}

main = toTry `catch` handler

toTry :: IO ()     
toTry = do (fileName:_) <- getArgs     
           contents <- readFile fileName     
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"     
    
handler :: IOError -> IO ()     
handler e     
    | isDoesNotExistError e =   
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
    | otherwise = ioError e
    