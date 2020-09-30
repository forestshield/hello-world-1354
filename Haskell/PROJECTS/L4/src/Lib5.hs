module Lib5
    ( someFuncLib5
--    , pathFileOne
--    , pathFileTwo
    ) where

import Lib
import Lib4
import Data.List
import Data.Char
import System.Info 

--import System.Info.Extra
--import System.Directory (getHomeDirectory)

import System.Directory 
import System.FilePath (joinPath, splitPath)
import System.Environment
import Data.Time
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Array
import Data.Complex
import Data.Graph
import qualified Data.HashSet as HS 
import qualified Data.HashMap.Lazy as HML
import Data.Version

--import Happstack.Server.Env
--import System.Environment

{-
import System.Environment
import System.Directory
import Data.Time
import Happstack.Server.Env
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)
import Yesod
-}


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

  putStrLn "=========================== Heathrow to London =================="
  
  putStrLn "=========================== Checking OS =================="
  funcCheckOS
  
  putStrLn "---------- IO FilePath & System.Directory ---------"
  specSh2 (getFullPath "~Haskell/" >>= print) "getFullPath \"~Haskell/\"" "getFullPath"
  specSh2 (getHomeDirectory >>= print) "" "getHomeDirectory, is an action, NOT A FUNTION"
  specSh2 (getUserDocumentsDirectory >>= print) "" "getUserDocumentsDirectory"

  putStrLn "=========================== Simple Examples =================="
  putStrLn "https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples#simple-application"
  putStrLn "\n--- getCurrentTime >>= print ---"
  func54main
  putStrLn "\ngetCurrentDirectory >>= print\ngetHomeDirectory >>= print\ngetUserDocumentsDirectory >>= print"
  func55main
  putStrLn "\n ----------------- Lists, list = [1,2,3,4,5]  -------------------"
  func56main
  putStrLn "\n ------------- Tuples, tuple = (1, 2), tuple3 = (1, 2, 3) ----------"
  func57main
  putStrLn "\n -------------- Data.List  -------------------"
  func58main
  putStrLn "\n -------------- Data.Char  -------------------"
  func59main
  putStrLn "\n -------------- Data.Map  -------------------"
  func60main
  putStrLn "\n -------------- Data.Set  -------------------"
  func61main
  putStrLn "\n -------------- Data.Array  -------------------"
  func62main
  putStrLn "\n -------------- Data.Complex  -------------------"
  func63main
  putStrLn "\n -------------- Data.HashSet  -------------------"
  func64main
  putStrLn "\n -------------- Data.HashMap  -------------------"
  func65main
  putStrLn "\n -------------- Data.Graph  -------------------"
  func66main


--  putStrLn "\n ----------------- Lists, list = [1,2,3,4,5]  -------------------"
--  func59main











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

-- ================================== Heathrow to London ===============================
--    A --50-- A1-- 5-- A2--40--A3--10--A4 
--             |        |       |       |
--             30       20      25      0
--             |        |       |       |
--    B --10-- B1--90-- B2--2 --B3--8 --B4 
--
--      A node is either a normal node and has information about the road that leads to the 
--      other main road and the road that leads to the next node or an end node, which only 
--      has information about the road to the other main road. A road keeps information about 
--      how long it is and which node it points to. For instance, the first part of the road 
--      on the A main road would be Road 50 a1 where a1 would be a node Node x y, where x and 
--      y are roads that point to B1 and A2
data Node = Node Road Road | EndNode Road
data Road = Road Int Node

--      Another way would be to use Maybe for the road parts that point forward. Each node has 
--      a road part that point to the opposite road, but only those nodes that aren't the end 
--      ones have road parts that point forward
--data Node = Node Road (Maybe Road)  
--data Road = Road Int Node
---
-- sections 50,10,30,  5,90,20,  40,2,25, and 10,8,0
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
type RoadSystem = [Section]
---
heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]
---
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]
---
--      Our function, we'll call it optimalPath should thus have a type declaration of 
--      optimalPath :: RoadSystem -> Path. If called with the road system heathrowToLondon, 
--      it should return the following path:
--      [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8)] 
---
--      Hint: it will be useful because (Path, Path) -> Section -> (Path, Path) can be used as 
--      the binary function for a left fold, which has to have a type of a -> b -> a
---
roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =   
    let priceA = sum $ map snd pathA  
        priceB = sum $ map snd pathB  
        forwardPriceToA = priceA + a  
        crossPriceToA = priceB + b + c  
        forwardPriceToB = priceB + b  
        crossPriceToB = priceA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                        then (A,a):pathA  
                        else (C,c):(B,b):pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  
                        then (B,b):pathB  
                        else (C,c):(A,a):pathA  
    in  (newPathToA, newPathToB)
---
rsHL1 = roadStep ([], []) (head heathrowToLondon)  -- ([(C,30),(B,10)],[(B,10)])
---
--      Optimization tip: when we do priceA = sum $ map snd pathA, we're calculating the price 
--      from the path on every step. We wouldn't have to do that if we implemented roadStep as 
--      a (Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int) function where the 
--      integers represent the best price on A and B.
---
optimalPath :: RoadSystem -> Path  
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem  
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)  
            then reverse bestAPath  
            else reverse bestBPath
---
rsHL2 = optimalPath heathrowToLondon  -- [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]
---
groupsOf :: Int -> [a] -> [[a]]  
groupsOf 0 _ = undefined  
groupsOf _ [] = []  
groupsOf n xs = take n xs : groupsOf n (drop n xs)
---
rsHL3 = groupsOf 3 [1..10]            -- [[1,2,3],[4,5,6],[7,8,9],[10]]
---
--import Data.List
--  
func45main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "The price is: " ++ show pathPrice
---
-- file path.txt --
-- 50\n10\n30\n5\n90\n20\n40\n2\n25\n10\n8\n0\n
-- cat paths.txt | runhaskell heathrow.hs  
-- The best path to take is: BCACBBC  
-- The price is: 75

-- !!! ======================= Functors again ========================= !!!
--    Functors are things that can be mapped over, like lists, Maybes, trees, and such. 
--    In Haskell, they're described by the typeclass Functor, which has only one typeclass 
--    method, namely fmap, which has a type of
--    fmap :: (a -> b) -> f a -> f b. 
--    It says: give me a function that takes an a and returns a b and a box with 
--    an a (or several of them) inside it and I'll give you a box with a b 
--    (or several of them) inside it. It kind of applies the function to the element 
--    inside the box.
--------------
--    A more correct term for what a functor is would be computational context. The context 
--    might be that the computation can have a value or it might have failed 
--    (Maybe and Either a) or that there might be more values (lists), stuff like that.
--------------
--    If we want to make a type constructor an instance of Functor, it has to have a kind 
--    of * -> *, which means that it has to take exactly one concrete type as a type parameter. 
--    For example, Maybe can be made an instance because it takes one type parameter to produce 
--    a concrete type, like Maybe Int or Maybe String. If a type constructor takes two parameters,
--    like Either, we have to partially apply the type constructor until it only takes one type 
--    parameter. So we can't write instance Functor Either where, but we can write instance 
--    Functor (Either a) where and then if we imagine that fmap is only for Either a, it would 
--    have a type declaration of fmap :: (b -> c) -> Either a b -> Either a c. As you can see, 
--    the Either a part is fixed, because Either a takes only one type parameter, whereas just 
--    Either takes two so fmap :: (b -> c) -> Either b -> Either c wouldn't really make sense.
---------------
--    We've learned by now how a lot of types (well, type constructors really) are instances of
--    Functor, like [], Maybe, Either a and a Tree type that we made on our own. We saw how we 
--    can map functions over them for great good. In this section, we'll take a look at two more
--    instances of functor, namely IO and (->) r
---------------
--    If some value has a type of, say, IO String, that means that it's an I/O action that, when
--    performed, will go out into the real world and get some string for us, which it will yield
--    as a result. We can use <- in do syntax to bind that result to a name. We mentioned that 
--    I/O actions are like boxes with little feet that go out and fetch some value from the 
--    outside world for us. We can inspect what they fetched, but after inspecting, we have to 
--    wrap the value back in IO. By thinking about this box with little feet analogy, we can 
--    see how IO acts like a functor.
--    Let's see how IO is an instance of Functor. When we fmap a function over an I/O action, 
--    we want to get back an I/O action that does the same thing, but has our function applied 
--    over its result value

--------------- this is how it is defined in GHC.Base ---------
--instance Functor IO where  
--    fmap f action = do
--        result <- action
--        return (f result)

----
func46main = do line <- getLine
                let line' = reverse line
                putStrLn $ "You said " ++ line' ++ " backwards!"
                putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"

--- same code, but rewritten with fmap
func47main = do line <- fmap reverse getLine
                putStrLn $ "You said " ++ line ++ " backwards!"
                putStrLn $ "Yes, you really said" ++ line ++ " backwards!"
--------------------
--    Just like when we fmap reverse over Just "blah" to get Just "halb", we can fmap reverse 
--    over getLine. getLine is an I/O action that has a type of IO String and mapping reverse 
--    over it gives us an I/O action that will go out into the real world and get a line and 
--    then apply reverse to its result. Like we can apply a function to something that's inside
--    a Maybe box, we can apply a function to what's inside an IO box, only it has to go out into
--    the real world to get something. Then when we bind it to a name by using <-, the name will
--    reflect the result that already has reverse applied to it.
---
--    The I/O action fmap (++"!") getLine behaves just like getLine, only that its result 
--    always has "!" appended to it!
---


-- ================================ System Info =====================================
func48main = do
    print os
    print arch
    print compilerName
    print compilerVersion


{-
"darwin"
"x86_64"
"ghc"
Version {versionBranch = [8,8], versionTags = []}
-}

--rsSI1 = isWindows
--rsSI2 = isMac

-------------------------------------
--- getHomeDirectory is not a function but an IO action so you have to unpack it 
--  within another IO action first.
--import System.Directory (getHomeDirectory)
--import System.FilePath (joinPath, splitPath)
---getHomeDirectory :: IO FilePath
--  this func is replacing "~" with "User/admin1" on this computer (MacOSX 10.11.6)
getFullPath :: String -> IO FilePath
getFullPath s = do
  homeDir <- getHomeDirectory
  if "~" `isPrefixOf` s
    then return (joinPath [homeDir, tail s])
  else return s

--getHomeDir :: IO FilePath
getHomeDir = do
  homeDir <- getHomeDirectory
  return homeDir



-- ================================ Simple Examples =====================================
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples

----
--import System.Info
func50main = do
    print os
    print arch
    print compilerName
    print compilerVersion

---
--import System.Environment
func51main = do
    getArgs >>= print
    getProgName >>= print
    getEnvironment >>= print    

--- System environment for web application
{-
import Happstack.Server.Env
import System.Environment
func52main = do
    environment <- getEnvironment
    simpleHTTP nullConf $ ok $ show environment
-}

{-
--- Streaming HTTP conduit
import Network.HTTP.Conduit
import Control.Monad.IO.Class (liftIO)
func53main = withManager $ \manager -> do
    request <- parseUrl "http://www.winsoft.sk"
    liftIO $ print request
    response <- httpLbs request manager
    liftIO $ print response      
-}

--import Data.Time
func54main = getCurrentTime >>= print             -- 2020-09-17 16:11:09.101648 UTC

-- Directories
--import System.Directory
func55main = do
    getCurrentDirectory >>= print 
    getHomeDirectory >>= print
    getUserDocumentsDirectory >>= print

{-
--- Yesod version
import Yesod
main = putStrLn yesodVersion
-}

{-
--- Yesod application
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
data WebApp = WebApp

instance Yesod WebApp

mkYesod "WebApp" [parseRoutes|
  / HomeR GET
|]

getHomeR = defaultLayout [whamlet|
  <div>Hello, world!
|]

main = warpEnv WebApp
-}

{-
--- Snap application
{-# LANGUAGE OverloadedStrings #-}

import Snap.Http.Server.Env
import Snap.Core

main = httpServe defaultConfig $ writeBS "Hello, world!"
Happstack application
import Happstack.Server.Env

main = simpleHTTP nullConf $ ok "Hello, world!"
-}

{-
--- JavaScript minification
{-# LANGUAGE OverloadedStrings #-}

import Text.Jasmine
import Data.ByteString.Lazy.Char8

main = print $ unpack $ minify "function test() { alert('Hello, world!'); }"
-}

--- Lists ---------
list = [1, 2, 3, 4, 5]

func56main = do
    print list

    print $ head list
    print $ tail list
    print $ last list
    print $ init list

    print $ list !! 3
    print $ elem 3 list

    print $ length list
    print $ null list
    print $ reverse list

    print $ take 2 list
    print $ drop 2 list

    print $ minimum list
    print $ maximum list
    print $ sum list
    print $ product list

    print [1..10]
    print ['A'..'Z']
    print [2,4..20]

    print $ take 10 $ cycle [1..4]
    print $ map (+1) list

    print $ filter (>3) list
    print $ all even list
    print $ any odd list

    print $ foldr (+) 0 list
    print $ foldr1 (+) list

    print $ foldl (+) 0 list
    print $ foldl1 (+) list

    print $ scanr (+) 0 list
    print $ scanr1 (+) list

    print $ scanl (+) 0 list
    print $ scanl1 (+) list

    print $ take 10 $ repeat 0
    print $ replicate 10 0
    print $ drop 3 list

    print $ ['a', 'b'] ++ ['c']
    print $ zip [1, 2, 3] ['a', 'b', 'c']
    print $ unzip [(1, 'a'), (2, 'b'), (3, 'c')]
    print $ zipWith (+) [1, 2, 3] [4, 5, 6]
    print $ [(x, y) | x <- [1..5], y <- ['a'..'e']]

    print $ words "Hello world"
    print $ unwords ["Hello", "world"] 

--- Tuples ---
tuple = (1, 2)
tuple3 = (1, 2, 3)
first (a, _, _) = a
second (_, b, _) = b
third (_, _, c) = c

func57main = do
    print tuple
    print $ fst tuple
    print $ snd tuple
    print tuple3
    print $ first tuple3
    print $ second tuple3
    print $ third tuple3

--- Data.List ---
--import Data.List
func58main = do
    print $ intersperse '.' "Erik"
    print $ intercalate " " ["abc","efg","x"]
    print $ transpose ["abc","efg"]
    print $ subsequences "abc"
    print $ permutations "abc"
    print $ foldl' (+) 0 [1..1000000]
    print $ foldl1' (+) [1..1000000]
    print $ concat ["abc","efg"]
    print $ any (== 'a') ("abcd"  :: String)
    print $ all (== 'a') ("abcd" :: String) 
    print $ take 10 $ iterate (+1) 1
    print $ replicate 10 'x'
    print $ splitAt 3 "abcdefgh"
    print $ takeWhile (< 3) [1..]
    print $ span (< 3) [1..10]
    print $ break (> 3) [1..10]
    print $ stripPrefix "ab" "abcdefg"
    print $ isPrefixOf "ab" "abcdefg"
    print $ elem 'c' ("abcdefg" :: String)
    print $ lookup 'c' [('a', 1), ('b', 2), ('c', 3)]
    print $ find (> 2) [1..]
    print $ partition (> 2) [1..10]
    print $ nub [1, 1, 3, 2, 1, 2, 4, 6]
    print $ sort [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
    print $ elemIndex 2 [1, 2, 3, 4, 2]
    print $ elemIndices 2 [1, 2, 3, 4, 2]
    print $ findIndex (< 3) [1, 2, 3, 4, 2]
    print $ findIndices (< 3) [1, 2, 3, 4, 2]

--- Data.Char ---
--import Data.Char
func59main = do
    print $ isAlpha 'c'
    print $ isDigit '4'
    print $ toUpper 'a'
    print $ toLower 'E'
    print $ digitToInt '2'
    print $ intToDigit 9
    print $ intToDigit 12
    print $ ord('A')
    print $ chr(61)

 
--- Data.Map ---
--import qualified Data.Map as Map
phoneBook = Map.fromList [(1234, "Erik"), (5678, "Patrik")]
func60main = do
    print phoneBook
    print $ Map.lookup 1234 phoneBook
    print $ (Map.empty :: Map.Map Int Int)
    print $ Map.singleton 3 5
    print $ Map.insert 1 "abc" Map.empty
    print $ Map.null phoneBook
    print $ Map.size phoneBook
    print $ Map.toList phoneBook
    print $ Map.keys phoneBook
    print $ Map.elems phoneBook

--- Data.Set ---
--import qualified Data.Set as Set
set = Set.fromList "erik salaj"
func61main = do
    print set
    print $ Set.null set
    print $ Set.size set
    print $ Set.member 'a' set

--- Data.Array ---
--import Data.Array
myArray = array (1, 3) [(1, "a"), (2, "b"), (3, "c")]
func62main = do
    print myArray
    print $ myArray ! 2
    print $ bounds myArray
    print $ indices myArray
    print $ elems myArray
    print $ assocs myArray

--- Data.Complex ---
--import Data.Complex
number = 3 :+ 4
func63main = do
    print number
    print $ realPart number
    print $ imagPart number
    print $ polar number
    print $ magnitude number
    print $ phase number
    print $ conjugate number


-- ================= see Data.HashSet and Data.HashMap samples in "stand_alone" folder ===
--- Data.HashSet ---
--import Prelude hiding (null, map, filter)
--import Data.HashSet
--import Data.Char
hashSet = HS.fromList ['a', 'b', 'c']
func64main = do
    print $ hashSet                     -- fromList "abc"      
    print $ HS.null hashSet             -- False                              
    print $ HS.size hashSet             -- 3                    
    print $ HS.member 'a' hashSet       -- True                    
    print $ HS.member 'e' hashSet       -- False                    
    print $ HS.insert 'd' hashSet       -- fromList "abcd"          
    print $ HS.delete 'b' hashSet       -- fromList "ac"              
    print $ HS.map (toUpper) hashSet    -- fromList "ABC"           
    print $ HS.filter (> 'a') hashSet   -- fromList "bc"               

--- Data.HashMap ---
--import Prelude hiding (null, lookup, map, filter)
--import Data.HashMap.Lazy
--import Data.Char
hashMap = HML.fromList [(1 :: Int, 'a'), (2, 'b'), (3, 'c')]
func65main = do
    print $ hashMap                         -- fromList [(1,'a'),(2,'b'),(3,'c')]
    print $ HML.keys hashMap                -- [1,2,3]                                
    print $ HML.elems hashMap               -- "abc"                                            
    print $ HML.null hashMap                -- False                                                
    print $ HML.size hashMap                -- 3                                                 
    print $ HML.member 1 hashMap            -- True                                   
    print $ HML.member 5 hashMap            -- False                                    
    print $ HML.lookup 1 hashMap            -- Just 'a'                                     
    print $ HML.lookup 5 hashMap            -- Nothing                                       
    --print $ hashMap ! 1                   -- 'a'                           
    print "this line does not compile: 'print $ hashMap ! 1', looks like it is related to quilified issue"
    print $ HML.lookupDefault 'N' 5 hashMap -- 'N'                                                      
    print $ HML.insert 4 'd' hashMap        -- fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d')]           
    print $ HML.delete 2 hashMap            -- fromList [(1,'a'),(3,'c')]                         
    print $ HML.map (toUpper) hashMap       -- fromList [(1,'A'),(2,'B'),(3,'C')]                    
    print $ HML.filter (> 'a') hashMap      -- fromList [(2,'b'),(3,'c')]                            

--- Data.Graph ---
--import Data.Graph
graph = buildG (1, 6) [(1, 2), (1, 3), (2, 4), (5, 6)]
func66main = do
    print graph
    print $ vertices graph
    print $ edges graph
    print $ edges $ transposeG graph

    print $ outdegree graph
    print $ indegree graph

    print $ topSort graph
    print $ reachable graph 1

    print $ path graph 1 4
    print $ path graph 1 5

    print $ components graph
    print $ scc graph
    print $ bcc graph

    print $ dff graph
    print $ dfs graph [2]

--- Data.Version ---
--import Data.Version
func676main = print $ showVersion
    Version {
        versionBranch = [1, 2, 3, 4],
        versionTags = ["Tag1", "Tag2", "Tag3"] -- Deprecated: "See GHC ticket #2496"
    }