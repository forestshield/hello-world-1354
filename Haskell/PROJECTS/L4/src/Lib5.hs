module Lib5
    ( someFuncLib5
    ) where

import Lib4
import Data.List

{-
import Data.Function
import Data.Char
import qualified GHC.Unicode as U 
import qualified Data.Map as Map 
import qualified Data.Set as Set
import Data.String
import Data.Int
--import GHC.Int
import Data.Char (toUpper)
import Control.Monad 
import System.IO
import System.Directory
import System.Environment
import System.Random
import System.IO.Error
import Control.Exception
--import Control.Exception.Base
--import Data.Array
import Data.Maybe
-}

-----------------------------
someFuncLib5 :: IO ()
someFuncLib5 = do
  putStrLn "\n----------- Lib5 -------------------------"
  putStrLn "=================== Functionaly Solving Problems ===================================="
  specShow ((solveRPN "10 4 3 + 2 * -"), (solveRPN' "90 34 12 33 55 66 + * - + -"), 
            (solveRPN' "2 3 +"), (solveRPN "90 34 12 33 55 66 + * - +"))
           "\nsolveRPN \"10 4 3 + 2 * -\"\nsolveRPN' \"90 34 12 33 55 66 + * - + -\"\n\
           \solveRPN' \"2 3 +\"\nsolveRPN \"90 34 12 33 55 66 + * - +\""
           "Reverse Polish Notation"
  specShow ((solveRPNB "2.7 ln"), (solveRPNB "10 10 10 10 sum 4 /"), 
            (solveRPNB "10 10 10 10 10 sum 4 /"), 
            (solveRPNB "10 2 ^"), (solveRPNB "43.2425 0.5 ^" ))
           "\nsolveRPNB \"2.7 ln\"\nsolveRPNB \"10 10 10 10 sum 4 /\"\n\
           \solveRPNB \"10 10 10 10 10 sum 4 /\"\nsolveRPNB \"10 2 ^\"\nsolveRPNB \"43.2425 0.5 ^\" \n"
           "better RBN calculator solveRPNB"

--  putStr $ show $ "Abrakadabra" `compare` "Zebra"
--  putStrLn ",  \"Abrakadabra\" `compare` \"Zebra\"" -- LT
{-  
  putStrLn "\n------------- fromIntegral ---------------------" 
  putStr $ show $ fromIntegral (minBound :: Int16) + 3.2       -- -32764.8
  putStrLn ", fromIntegral (minBound :: Int16) + 3.2"
  putStrLn "But, (minBound :: Int16) + 3.2 -- compiler error"
  
  specShow (fromIntegral (minBound :: Int16) + 3.2)
           ", fromIntegral (minBound :: Int16) + 3.2 \n\
           \But, (minBound :: Int16) + 3.2 -- compiler error"
           "fromIntegral"
  putStrLn "\n==================== import Data.List ===================="
  specSh2 (func28main) "\nfunc28main, it also using its own IO output" "non repetivive results of 2 sets, every time new, using 2 generators"
-}
{-
  specShow (()
           ,()
           ,()
           ,()
           ,()
           ,()
           )
           "\n\n\n\
           \\n\n\n"
           ""
  specShow ((), (), (), 
            (), (), ())
           "\n\n\n\
           \\n\n\n"
           ""
  specShow ()
           "\n\
           \"
           ""
  specSh2 ()
           "\n\
           \"
           ""
-}

{-
specShow :: Show a => a -> String -> String -> IO ()
--specShow :: a -> String -> String -> IO () -- does not work
specShow a b c = do
  specHeader c    
  putStr $ show (a)
  putStrLn b 
--specHeader2
specHeader2 :: String -> IO ()
specHeader2 a = do  
  putStr "\n------------- "  
  putStr a 
  putStrLn " -------------"
--specHeader
specHeader :: String -> IO ()
specHeader  a | a /= "" = specHeader2 a | otherwise = return () -- from Control.Monad
----------------------------------
-- specS2 ---
specSh2 :: IO a -> String -> String -> IO ()
specSh2 a b c = do
  specHeader c 
  a 
  putStrLn b 
-}

-- =================== Functionaly Solving Problems ====================================
-- http://learnyouahaskell.com/functionally-solving-problems

--- Reverse Polish Notation ---

-- sketch #1
--import Data.List  
--solveRPN :: (Num a) => String -> a  
--solveRPN expression = head (foldl foldingFunction [] (words expression))  
--    where   foldingFunction stack item = ...        

--- 
solveRPN :: (Num a, Read a) => String -> a
solveRPN expression = head (foldl foldingFunction [] (words expression))
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            --foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction xs numberString = read numberString:xs

-- sketch #2, same as #1, but point-free style
--solveRPN :: (Num a) => String -> a
--solveRPN = head . foldl foldingFunction [] . words
--    where   foldingFunction stack item = ...        

--- point-free style implementation
solveRPN' :: (Num a, Read a) => String -> a
solveRPN' = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            --foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction xs numberString = read numberString:xs

---
rsRPN1 = solveRPN "10 4 3 + 2 * -"                -- -4       -- 10 - (4 + 3) * 2
rsRPN2 = solveRPN "2 3 +"                         -- 5        -- 3 + 2
rsRPN3 = solveRPN "90 34 12 33 55 66 + * - +"     -- -3947    -- 12 - (66 + 55) * 33 + 34
rsRPN4 = solveRPN "90 34 12 33 55 66 + * - + -"   -- 4037     -- 90 - (12 - (66 + 55) * 33 + 34)
rsRPN5 = solveRPN' "90 3 -"                       -- 87       -- 3 - 90
rsRPN6 = solveRPN' "90 34 12 33 55 66 + * - + -"  -- 4037     -- 90 - (12 - (66 + 55) * 33 + 34)
rsRPN7 = solveRPN "3 1 + -"                       -- *** Exception: Prelude.read: no parse
rsRPN8 = solveRPN "4 + -"                         -- *** Exception: Prelude.read: no parse
rsRPN9 = solveRPN "5 + 1 -"                       -- *** Exception: Prelude.read: no parse
---

--- step by step analyzing in details our implementaion ---
rsRPN10 = words "90 34 12 33 55 66 + * - + -"  -- ["90","34","12","33","55","66","+","*","-","+","-"]

---
foldingFunction :: (Num a, Read a) => [a] -> [Char] -> [a]
--foldingFunction []
foldingFunction (x:y:ys) "*" = (x * y):ys
foldingFunction (x:y:ys) "+" = (x + y):ys
foldingFunction (x:y:ys) "-" = (y - x):ys
--foldingFunction (x:y:ys) "/" = (y / x):ys
foldingFunction xs numberString = read numberString:xs
---
rsRPN11 = foldl foldingFunction [] (words "1 2 +")                          -- [3]
rsRPN12 = foldl foldingFunction [] (words "90 34 12 33 55 66 + * - + -")    -- [4037]
---
rsRPN13 = head (foldl foldingFunction [] (words "1 2 +"))                        -- 3
rsRPN14 = head (foldl foldingFunction [] (words "90 34 12 33 55 66 + * - + -"))  -- 4037

----------------------------------------------------
--import Data.List  
solveRPNB :: String -> Float  
solveRPNB = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs 
--      One thing to note about this function is that it's not really fault tolerant. 
--      When given input that doesn't make sense, it will just crash everything. We'll make 
--      a fault tolerant version of this with a type declaration of 
--      solveRPN :: String -> Maybe Float once we get to know monads. We could make one 
--      right now, but it would be a bit tedious because it would involve a lot of checking 
--      for Nothing on every step. If you're feeling up to the challenge though, you can go 
--      ahead and try it! Hint: you can use reads to see if a read was successful or not.            
---
rsRPN15 = solveRPNB "2.7 ln"                     -- 0.9932518
rsRPN16 = solveRPNB "10 10 10 10 sum 4 /"        -- 10.0
rsRPN17 = solveRPNB "10 10 10 10 10 sum 4 /"     -- 12.5  
rsRPN18 = solveRPNB "10 2 ^"                     -- 100.0
--      Notice that we can include floating point numbers in our expression because read 
--      knows how to read them.
rsRPN19 = solveRPNB "43.2425 0.5 ^"              -- 6.575903