module Lib
    ( someFunc
    ) where

import Data.String

someFunc :: IO ()
someFunc = do
    putStrLn "someFunc"
    putStrLn $ show (1) 
    --putStrLn $ show (length "a")  -- does not work, if "- OverloadedStrings" is on 
    putStrLn $ show (length' "a") 
    putStrLn $ show (length'' "a") 
    putStrLn $ show (length'' "") 
    putStrLn $ show $ tell [1]
    --putStrLn $ show $ tell []
    --print (tell [])

-- does not work, if "- OverloadedStrings" is off ?!  
boomBangs :: (Integral a1, Data.String.IsString a2) => [a1] -> [a2]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
--boomBangs xs = [ if x < 10 then strBoom else strBoom | x <- xs, odd x]
--oddEven
oddEven :: (Integral a1, Data.String.IsString a2) => [a1] -> [a2]
oddEven xs = [if even x then "even" else "odd" | x <- xs]



-- length - the original one
--length           :: [a] -> Int
--length []        =  0
--length (_:l)     =  1 + length l

-- length' ---
--    This function replaces every element of a list with 1 and then sums that up
length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]  

-- length''
length'' :: (Num b) => [a] -> b  
length'' [] = 0  
length'' (_:xs) = 1 + length'' xs

-- tell  
tell          :: (Show a) => [a] -> String  
tell []       = "The list is empty"  
tell (x:[])   = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_)  = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

