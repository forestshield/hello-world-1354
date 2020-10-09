module Lib5
    ( someFuncLib5
--    , pathFileOne
--    , pathFileTwo
    ) where

import ExTT 
import Lib
import Lib4
import GHCOptions
import Data.List
import Data.Char
import System.Info 

--import System.Info.Extra
--import System.Directory (getHomeDirectory)

import Control.Exception
--import Data.Typeable (typeOf)
import System.Directory 
import System.FilePath (joinPath, splitPath)
import System.Environment
import Data.Time
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Array
import Data.Complex
import qualified Data.Graph as DG
import qualified Data.HashSet as HS 
import qualified Data.HashMap.Lazy as HML
import Data.Version
import Happstack.Server
import System.Process 
--import qualified Yesod as Y
import Yesod
import Text.Jasmine
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit 
import Control.Monad.IO.Class (liftIO)
--import Snap.Http.Server.Env
import Snap.Http.Server
import Snap.Core
import System.IO 
import System.Random
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8  as BC8
import qualified Data.ByteString.Base64 as BS64 
import Data.Aeson
import GHC.Generics
import Control.Applicative
import Text.Email.Validate
import Control.Concurrent
import System.CPUTime
import Debug.Trace
import Data.Unique
import Test.QuickCheck
import qualified Data.ByteString.UTF8 as UTF8
import Data.Typeable
import qualified Data.Serialize as SRZ
import Data.Word
import Data.Tree
import Numeric
import Data.IORef
import qualified Control.Concurrent.STM as CCSTM
import Data.Tuple
import System.ByteOrder 
import qualified Text.Bytedump as TBD
import System.Arch
import System.Endian
import Network.HostName
import Prelude hiding (null)
import Data.UUID
import Data.UUID.V1
import Data.UUID.V3 as V3
import Data.UUID.V4
import Data.UUID.V5 as V5
import Data.Digest.Pure.SHA
import Data.Digest.Pure.MD5
import Language.JavaScript.Parser
import Crypto.Hash.MD2 as MD2
import Crypto.Hash.MD4 as MD4
import Crypto.Hash.MD5 as MD5
import Crypto.Hash.RIPEMD160 as RIPEMD160
import Crypto.Hash.SHA1 as SHA1
import Crypto.Hash.SHA224 as SHA224
import Crypto.Hash.SHA256 as SHA256
import Crypto.Hash.SHA384 as SHA384
import Crypto.Hash.SHA512 as SHA512
import Crypto.Hash.Tiger as Tiger
import Crypto.Hash.Whirlpool as Whirlpool
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Codec.Compression.Zlib
import Codec.Compression.GZip
import qualified Linear as LN 
--import qualified Data.Matrix as DM 
--import qualified Numeric.LinearAlgebra as NLA
--import Numeric.LinearAlgebra
--import Physics.Learn.QuantumMat
import Data.Matrix
import Language.C
import Data.Maybe
--import Data.Char
import Data.Either
import Prelude hiding (error)
--import Data.List
import Data.Ord


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
  specSh2 (func50C_main) "" "\n--- Yesod version ---" 
  putStrLn "\ngetCurrentDirectory >>= print\ngetHomeDirectory >>= print\ngetUserDocumentsDirectory >>= print"
  func55main 
  putStrLn "\n ----------------- getEnvironment  -------------------"
  specSh2 (getEnvironment >>= print) "" ""  

  putStrLn "\n ----------------- Happstack.Server  -------------------"
  --callCommand  ("curl -v http://localhost")  
  specSh2 (testExceptionType (callCommand  ("\ncurl http://localhost"))) "" "using \"curl http://localhost\""
  --callCommand  ("curl -v http://b-ok.cc")  
  --specSh2 (testExceptionType (callCommand  ("\ncurl http://b-ok.cc"))) "" "using \"curl http://b-ok.cc\""

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
  putStrLn "\n ----- Unicode Strings (withCP65001, which is UTF-8) ------"
  putStrLn "λλλ, 2.0 == 2.1"      -- λλλ, 2.0 == 2.1
  putStrLn "κόσμε"
  putStrLn "→"
  putStrLn "☀☁☂☃☄"
  specSh2 (func5D_main) "" " JavaScript minification "
  specSh2 (func5E_main) "http://www.winsoft.sk" " Simple HTTP conduit "
  putStrLn "\n"
  specSh2 (testExceptionType (func5H_main)) "http://doesNotExist" " Simple HTTP conduit "
  specSh2 (testExceptionType (func5G_main)) "http://localhost" " Simple HTTP conduit "
  specSh2 (func81main) "" "show and read"
  specSh2 (func8A_main) "func8A_main" "...Files..."
  specSh2 (func89main) "(randomRIO (1, 100)" "...Random..."
  specSh2 (func90main) "unpack encode decode" "...Data.ByteString.Base16..."
  specSh2 (func91main) "unpack encode decode" "...Data.ByteString.Base64..."  
  specSh2 (func92main) "instance FromJSON instance ToJSON" "...JSON..."
  specSh2 (func93main) "instance FromJSON instance ToJSON" "...JSON..."  
  specSh2 (func94main) "eriksalaj@gmail.com" "Email validation"
  specSh2 (func95main) "" "Functor"
  specSh2 (func96main) "" "Applicative"  
  specSh2 (func97main) "" "Type class"
  specSh2 (func98main) "" "Threads"
  specSh2 (func99main) "" "CPU time"
  specSh2 (func100main) "" "External command"  
  specSh2 (func102main) "print $ (1 /) 2\nprint $ (/ 1) 2" "Section"
  specSh2 ((func101main))
           "\nprint $ trace \"Calling 1 + 1\" (1 + 1)\ntraceIO \"Calling 1 + 1\"\nprint $ traceShow (x, x + x) (x + x)"           
           "Debug.Trace"
  specSh2 (func103main) "N.B. Place 'main.c' file in the same folder where L4-exe is." "Compiling and running C application, main.c"
  specSh2 (func104main) "N.B. Place 'Main.java' file in the same folder where L4-exe is." "'Compiling' and running Java application, Main.java"
  specSh2 (func105main) "N.B. Place 'hello.py' file in the same folder where L4-exe is." "'Compiling' and running Python application, hello.py"
  specSh2 (func106main) "" "Unuque values"
  specSh2 (func107main) "" "Test.QuickCheck"
  specSh2 (func108main) "" "Data.ByteString.UTF8"
  specSh2 (func109main) "" "Type representations, import Data.Typeable"
  specSh2 (func110main) "" "import Numeric"
  specSh2 (func111main) "" "import Data.Tree"
  specSh2 (func112main) "" "Binary serialization, - serialize, - cereal"
  specSh2 (func113main) "" "Integral"
  specSh2 (func114main) "" "Data.IORef"
  specSh2 (func115main) "" "import Control.Concurrent.STM, - stm"
  specSh2 (func117main) "" "import System.ByteOrder, - byteorder"
  specSh2 (func118main) "" "import Text.Bytedump, - bytedump"
  specSh2 (func119main) "" "Data.UUID, Data.UUID.V1, Data.UUID.V1, Data.UUID.V3, Data.UUID.V4, Data.UUID.V5, - uuid"
  specSh2 (func120main) "" "import System.Arch, import System.Endian, - cpu"
  specSh2 (func121main) "" "import Network.HostName, - hostname"
  specSh2 (func122main) "" "import Data.Digest.Pure.SHA, - SHA"
  specSh2 (func123main) "" "import Data.Digest.Pure.MD5, - pureMD5"
  --specSh2 (func124main) "" "import Data.Text.Punycode, - punycode" -- not on Stackage
  specSh2 (testExceptionType (func128main)) "JavaScript Parser done" "import Language.JavaScript.Parser, - language-javascript"
  specSh2 (func129main) "" "import Crypto.Hash, - cryptohash"
  specSh2 (testExceptionType (func130main)) "" "import Data.Algorithm.DiffOutput, \
    \import Data.Algorithm.Diff - Diff"
  specSh2 (func131main) "" "import Codec.Compression.Zlib, - zlib"
  specSh2 (func132main) "" "import Codec.Compression.GZip, - zlib"
  specSh2 (func133main) "" "import Linear, -import Linear Algebra, - linear"
  specSh2 (func134main) "" "import Data.Matrix, - matrix"
  specSh2 (func135main) "" "Tower of Hanoi"
  specSh2 (func136main) "It does not parse '//', '#', '/*' !!!" "C Parser, import Language.C, - language-c"
  specSh2 (func142main) "" "Fibonacci version: 1" 
  specSh2 (func139main) "" "Fibonacci version: 2" 
  specSh2 (func140main) "" "Fibonacci version: 3" 
  specSh2 (func141main) "" "Fibonacci version: 4" 
  specSh2 (func137main) "" "import Data.Either, import Prelude hiding (error)" 
  specSh2 (func143main) "" "Coin change, version 1" 
  specSh2 (func144main) "" "Coin change, version 2" 
  specSh2 (func145main) "" "Coin change, version 3" 
  specSh2 (func146main) "" "Queens" 
  -- function with GHCOptions --
  funcGHCOptions1

  --specSh2 (func10main) "" ""
--  putStr $ show $ "Abrakadabra" `compare` "Zebra"
--  putStrLn ",  \"Abrakadabra\" `compare` \"Zebra\"" -- LT
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
data Node1 = Node1 Road Road | EndNode1 Road
data Road = Road Int Node1

--      Another way would be to use Maybe for the road parts that point forward. Each node has 
--      a road part that point to the opposite road, but only those nodes that aren't the end 
--      ones have road parts that point forward
--data Node1 = Node1 Road (Maybe Road)  
--data Road = Road Int Node1
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
    print os               -- "darwin"                                                                                                    
    print arch             -- "x86_64"                                                                                                         
    print compilerName     -- "ghc"                                                                                                              
    print compilerVersion  -- Version {versionBranch = [8,8], versionTags = []}                                                                      

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
    then return (System.FilePath.joinPath [homeDir, tail s])
  else return s

--getHomeDir :: IO FilePath
getHomeDir = do
  homeDir <- getHomeDirectory
  return homeDir

-- ================= to run shell script =========================
--import System.Process 
func5A_main = callCommand "ls -la"
--func5B_main = callCommand "curl -v http://localhost"
--- trying here not to call localhost, because it might be not runnoing
func5B_main = callCommand "curl -v http://b-ok.cc"

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
--import Happstack.Server.Env
--import System.Environment
func52main = do 
    environment <- getEnvironment
    simpleHTTP nullConf $ ok $ show environment

--- Simple HTTP conduit ---
--import Network.HTTP.Conduit
--import qualified Data.ByteString.Lazy as L
func5E_main = simpleHttp "http://www.winsoft.sk" >>= L.putStr
func5G_main = simpleHttp "http://localhost" >>= L.putStr
func5H_main = simpleHttp "http://doesNotExist" >>= L.putStr

-- ====================== Exception Type Tester =========================
-- See file ExTT.hs
{-
testExceptionType :: IO () -> IO ()
testExceptionType thunk =  catch thunk handler
  where
    -- Catch All Exceptions -- It is not recommended in real life.
    handler :: SomeException -> IO ()
    handler (SomeException e) = putStrLn $ "I caught an exception.\nMessage =  " ++ show e ++ "\
    \\nType of exception = " ++ show (typeOf e)
-}
--- 
func5F_dangerous = do
    func5G_main             -- simpleHttp "http://localhost" >>= L.putStr
    testExceptionType (func5G_main)


{-
--- Streaming HTTP conduit
--import Network.HTTP.Conduit
--import Control.Monad.IO.Class (liftIO)
func53main = withManager $ \manager -> do
    request <- parseUrl "http://www.winsoft.sk"
    liftIO $ print request
    response <- httpLbs request manager
    liftIO $ print response      
-}

{-
--import Data.Conduit.Binary (sinkFile)
--import Network.HTTP.Conduit
--import qualified Data.Conduit as C
func70main :: IO ()
func70main = do
    request <- parseUrl "http://google.com/"
    withManager $ \manager -> do
        Response _ _ bsrc <- Network.HTTP.Conduit.http request manager
        bsrc C.$$ sinkFile "google.html"
-}

--import Data.Time
func54main = getCurrentTime >>= print             -- 2020-09-17 16:11:09.101648 UTC

-- Directories
--import System.Directory
func55main = do
    getCurrentDirectory >>= print 
    getHomeDirectory >>= print
    getUserDocumentsDirectory >>= print

--- Yesod version
--import Yesod
func50C_main = putStrLn yesodVersion

{-
--- Yesod application
--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE QuasiQuotes #-}
--{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
data WebApp = WebApp
instance Yesod WebApp
mkYesod "WebApp" [parseRoutes|
  / HomeR GET
|]            
getHomeR = defaultLayout [whamlet|
  <div>Hello, world!
|]
func125main = warpEnv WebApp
-}

--- Snap application
--{-# LANGUAGE OverloadedStrings #-}
--import Snap.Http.Server.Env
--import Snap.Core
func82main = httpServe defaultConfig $ writeBS "Hello, world!"
{-
-- this is output from ghci
λ> func82main
no port specified, defaulting to port 8000
Listening on http://0.0.0.0:8000
Can't open log file "log/access.log".
Can't open log file "log/error.log".
Exception: log/access.log: openFile: does not exist (No such file or directory)
Exception: log/error.log: openFile: does not exist (No such file or directory)
Logging to stderr instead. **THIS IS BAD, YOU OUGHT TO FIX THIS**

Logging to stderr instead. **THIS IS BAD, YOU OUGHT TO FIX THIS**


^C
Shutting down..
Interrupted.
-}

--- JavaScript minification
--{-# LANGUAGE OverloadedStrings #-}
--import Text.Jasmine
--import Data.ByteString.Lazy.Char8
func5D_main = print $ BL.unpack $ minify "function test() { alert('Hello, world!'); }"

--- Lists ---------
list = [1, 2, 3, 4, 5]
func56main = do
    print list

    print $ head list
    print $ tail list
    print $ last list
    print $ Prelude.init list

    print $ list !! 3
    print $ elem 3 list

    print $ length list
    print $ Data.List.null list
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
    print $ Data.List.transpose ["abc","efg"]
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
    print $ Data.List.find (> 2) [1..]
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
myArray = Data.Array.array (1, 3) [(1, "a"), (2, "b"), (3, "c")]
func62main = do
    print myArray
    print $ myArray Data.Array.! 2
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
graph = DG.buildG (1, 6) [(1, 2), (1, 3), (2, 4), (5, 6)]
func66main = do
    print graph
    print $ DG.vertices graph
    print $ DG.edges graph
    print $ DG.edges $ DG.transposeG graph

    print $ DG.outdegree graph
    print $ DG.indegree graph

    print $ DG.topSort graph
    print $ DG.reachable graph 1

    print $ DG.path graph 1 4
    print $ DG.path graph 1 5

    print $ DG.components graph
    print $ DG.scc graph
    print $ DG.bcc graph

    print $ DG.dff graph
    print $ DG.dfs graph [2]

--- Data.Version ---
--import Data.Version
func676main = print $ showVersion
    Version {
        versionBranch = [1, 2, 3, 4],
        versionTags = ["Tag1", "Tag2", "Tag3"] -- Deprecated: "See GHC ticket #2496"
    }

--- show and read
func81main = do
    print $ show 3
    print $ show [1, 2, 3]
    print $ show (1, False)
    print $ (read "34" :: Int)
    print $ (read "(1, False)" :: (Int, Bool))

{-
--- SQLite database ---
--{-# LANGUAGE OverloadedStrings #-}
--import Database.Sqlite
printRows stmt = do
    row <- step stmt
    if row == Done then
        return ()
    else do
        col <- column stmt 0
        print col
        printRows stmt
---
func83main = do
    conn <- open "database.db"

    stmt <- prepare conn "DROP TABLE IF EXISTS MyTable;"
    step stmt
    finalize stmt

    stmt <- prepare conn "CREATE TABLE IF NOT EXISTS MyTable (Name VARCHAR(20));"
    step stmt
    finalize stmt

    stmt <- prepare conn "INSERT INTO MyTable(Name) VALUES('Erik');"
    step stmt
    finalize stmt

    stmt <- prepare conn "INSERT INTO MyTable(Name) VALUES('Patrik');"
    step stmt
    finalize stmt

    stmt <- prepare conn "SELECT * FROM MyTable;"
    printRows stmt
    finalize stmt

    close conn
-}

-- ================================ Files ================================= --
-- stack new PACKAGE_NAME myfiles.hsfiles will create those files (and all directories automatically) 
-- according to your layout, if myfiles.hsfiles contains:
-- {-# START_FILE {{name}}.cabal #-}
-- name:                {{name}}
-- version:             0.1.0.0
-- or
-- {-# START_FILE package.yaml #-}
-- name:                {{name}}
-- version:             0.1.0.0
-- See some examples in this repository:
-- https://github.com/commercialhaskell/stack-templates

func8A_main = do
    writeFile "file.txt" "Hello, world!"
    readFile "file.txt" >>= print

--{-# START_FILE main.hs #-}            --  warning: [-Wunrecognised-pragmas]
func84main = readFile "file.txt" >>= putStr

--{-# START_FILE file.txt #-}
--Hello, world!

--{-# START_FILE main.hs #-}
func85main = do
    contents <- readFile "file.txt"
    putStr contents

--{-# START_FILE file.txt #-}
--Hello, world!

--{-# START_FILE main.hs #-}
--import System.IO
func86main = do
    handle <- openFile "file.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

--{-# START_FILE file.txt #-}
--Hello, world!

--{-# START_FILE main.hs #-}
--import System.IO
func87main = withFile "file.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStr contents

--{-# START_FILE file.txt #-}
--Hello, world!

func88main = do
    writeFile "file.txt" "Hello, world!"
    readFile "file.txt" >>= print

--- Random numbers ---
--import System.Random
func89main = (randomRIO (1, 100) :: IO Int) >>= print

--- Base16 encoding ---
--{-# LANGUAGE OverloadedStrings #-}
--import Data.ByteString.Base16
--import Data.ByteString.Char8
func90main = do
    print $ BC8.unpack $ BS16.encode "Hello, world!"
    print $ BS16.decode "48656c6c6f2c20776f726c6421"

--- Base64 encoding ---
--{-# LANGUAGE OverloadedStrings #-}
--import Data.ByteString.Base64
--import Data.ByteString.Char8
func91main = do
    print $ BC8.unpack $ BS64.encode "Hello, world!"
    print $ BS64.decode "SGVsbG8sIHdvcmxkIQ=="


--- JSON ---
--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE DeriveGeneric #-}
--import Data.Aeson
--import Data.ByteString.Lazy.Char8
--import GHC.Generics

data MyData = MyData { text :: String, number1 :: Int } deriving (Show, Generic)
instance FromJSON MyData
instance ToJSON MyData
myData = MyData "Hello" 123
func92main = do
    print myData
    print $ BL.unpack $ encode myData
    print $ (decode "{ \"number1\" : 123, \"text\" : \"Hello\" }" :: Maybe MyData)

--{-# LANGUAGE OverloadedStrings #-}
--import Data.Aeson
--import Control.Applicative
--import Data.ByteString.Lazy.Char8 hiding (empty)

data MyData2 = MyData2 { text2 :: String, number2 :: Int } deriving Show
instance ToJSON MyData2 where
    toJSON (MyData2 text2 number2) = object ["text" .= text2, "number2" .= number2]
instance FromJSON MyData2 where
    parseJSON (Object v) = MyData2 <$> v .: "text" <*> v .: "number2"
    parseJSON _          = empty
myData2 = MyData2 "Hello" 123
func93main = do
    print myData2
    print $ BL.unpack $ encode myData2
    print $ (decode "{ \"number2\" : 123, \"text2\" : \"Hello\" }" :: Maybe MyData2)

--- Email validation ---
--{-# LANGUAGE OverloadedStrings #-}
--import Text.Email.Validate
email = "eriksalaj@gmail.com"
func94main = do
    print $ isValid email
    print $ validate email
    print $ emailAddress email
    print $ canonicalizeEmail email

    let Just address = emailAddress email
    print $ localPart address
    print $ domainPart address
    
--- Functor ---
func95main = do
    print $ fmap (+ 1) Nothing
    print $ fmap (+ 1) $ Just 2

    print $ fmap (+ 1) [1, 2, 3]
    print $ fmap (* 2) (+ 5) 2 

--- Applicative ---
--import Control.Applicative
func96main = do
    print $ (pure 1 :: Maybe Int)

    print $ Just (+ 1) <*> Nothing
    print $ Just (+ 1) <*> Just 2

    print $ [(+ 1), (* 2)] <*> []
    print $ [(+ 1), (* 2)] <*> [1, 2, 3]

    print $ Just 1 <* Just 2
    print $ Just 1 *> Just 2

    print $ (+ 1) <$> Nothing
    print $ (+ 1) <$> Just 2

    print $ 1 <$ Nothing
    print $ 1 <$ Just 2

    print $ Nothing <**> Just (+ 2)
    print $ Just 1 <**> Just (+ 2)

    print $ liftA (+ 1) Nothing
    print $ liftA (+ 1) $ Just 2

    print $ liftA2 (+) Nothing Nothing
    print $ liftA2 (+) (Just 1) (Just 2)

    print $ (+) <$> Just 1 <*> Nothing
    print $ (+) <$> Just 1 <*> Just 2
   
--- Type class ---
class MyClass a where
    myFunc :: a -> String

instance MyClass Bool where
    myFunc n = "Bool: " ++ show n
instance MyClass Char where
    myFunc n = "Char: " ++ show n

myShow :: MyClass a => a -> String
myShow n = myFunc n

func97main = do
    print $ myFunc True
    print $ myFunc 'a'

    print $ myShow True
    print $ myShow 'a'

--- Record ---
data Person = Person { firstName :: String, lastName :: String } deriving Show
person = Person "Erik" "Salaj"
main = do
    print person
    print Person { firstName = "Erik", lastName = "Salaj" }
    print $ firstName person
    print $ lastName person

--- Threads ---
--import Control.Concurrent
func98main = do
    getNumCapabilities >>= print
    print rtsSupportsBoundThreads

    forkIO $ sequence_ $ replicate 3 $ do { print "Thread 1"; threadDelay 1 }
    forkIO $ sequence_ $ replicate 3 $ do { print "Thread 2"; threadDelay 1 }
    forkIO $ sequence_ $ replicate 3 $ do { print "Thread 3"; threadDelay 1 }
    threadDelay 10000

--- CPU time ---
--import System.CPUTime
func99main = do
    print cpuTimePrecision
    getCPUTime >>= print

--- External command ---
--import System.Process
func100main = do
    system "echo Hello, world!"

    system "uname -a"
    system "cat /proc/version"
    system "cat /proc/cpuinfo"
    system "lsb_release -a"

    system "ghc --version"
    system "cc --version"
    --system "java -version"
    system "ls -la"
    system "python --version"

--- Trace ---
--import Debug.Trace
func101main = do
    print $ Debug.Trace.trace "Calling 1 + 1" (1 + 1)

    traceIO "Calling 1 + 1"
    print $ 1 + 1

    let x = 1
    print $ traceShow (x, x + x) (x + x)

--- Sections ---
func102main = do
    print $ (1 /) 2
    print $ (/ 1) 2

--- C application
--{-# START_FILE main.hs #-}
--import System.Process
func103main = do
    system "cc main.c"
    system "./a.out"
-- N.B. Place 'main.c' file in the same folder where L4-exe is.
--  {-# START_FILE main.c #-}
--  #include <stdio.h>
--  int main() {
--     printf("Hello, world!\n");
--     return 0;
--  }

--- Java application ---
--{-# START_FILE main.hs #-}
--import System.Process
func104main = do
    system "javac Main.java"
    system "java Main"
-- N.B. Place 'Main.java' file in the same folder where L4-exe is.
-- {-# START_FILE Main.java #-}
-- public class Main {
--    public static void main(String[] args) {
--        System.out.println("Hello, world!");
--    }
-- }

--- Python application ---
--{-# START_FILE main.hs #-}
--import System.Process
-- N.B. Place 'helo.py' file in the same folder where L4-exe is.
func105main = system "python hello.py"
-- {-# START_FILE hello.py #-}
-- print "Hello, world!"

--- Unique values ---
--import Data.Unique
func106main = do
    unique <- newUnique
    print $ hashUnique unique

    unique <- newUnique
    print $ hashUnique unique

    unique <- newUnique
    print $ hashUnique unique

--- Automatic testing ---
--import Test.QuickCheck
check x = x == (reverse . reverse) x
func107main = do
    print stdArgs
    sample (Test.QuickCheck.vector 3 :: Gen [Int])
    sample (orderedList :: Gen [Int])

    quickCheck (Lib5.check :: [Int] -> Bool)
    verboseCheck (Lib5.check :: [Int] -> Bool)

--- UTF-8 ---
--{-# LANGUAGE OverloadedStrings #-}
--import qualified Data.ByteString.UTF8 as UTF8
--import Data.ByteString.Char8

func108main = do
    print "áéí"
    print $ UTF8.fromString "áéí"
    print $ UTF8.length "\195\161\195\169\195\173"
    print $ UTF8.toString "\195\161\195\169\195\173"
    putStrLn "áéí"
    putStrLn "→"
    putStrLn "λ>"
    
--- Type representations ---
--import Data.Typeable
func109main = do
    print $ typeOf 'a'
    print $ typeOf ("Hello, world!" :: String)
    print $ typeOf putStrLn
    print $ (cast True :: Maybe Int)
    print $ (cast True :: Maybe Bool)

--- Modules ---
{-
{-# START_FILE main.hs #-}
import Test
main = helloWorld

{-# START_FILE Test.hs #-}
module Test where
helloWorld = putStrLn "Hello, world!"
-}

--- Numeric ---
--import Numeric
--import Data.Char
func110main = do
    print $ showInt 123 ""
    print $ showHex 123 ""
    print $ showOct 123 ""
    print $ showIntAtBase 2 intToDigit 123 ""

    print $ showFloat 123.456 ""
    print $ showEFloat (Just 2) 123.456 ""
    print $ showFFloat (Just 2) 123.456 ""
    print $ showGFloat (Just 2) 123.456 ""
    print $ floatToDigits 10 123.456
    print $ floatToDigits 16 123.456

--- Data.Tree ---
--import Data.Tree
tree = Node "A" [Node "B" [], Node "C" [Node "D" [], Node "E" []]]
func111main = do
    print tree
    putStrLn $ drawTree tree
    putStrLn $ drawForest $ subForest tree

    print $ Data.Tree.flatten tree
    print $ levels tree

--- Binary serialization ---
--{-# LANGUAGE OverloadedStrings #-}
--import Data.Serialize
--import Data.Word
--import Data.ByteString.Char8
func112main = do
    print $ SRZ.encode (123 :: Word8)
    print $ (SRZ.decode "{" :: Either String Word8)

    print $ SRZ.encode (123 :: Word16)
    print $ (SRZ.decode "\NUL{" :: Either String Word16)

    print $ SRZ.encode 'a'
    print $ (SRZ.decode "a" :: Either String Char)

    print $ SRZ.encode ("abc" :: String)
    print $ (SRZ.decode "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETXabc" :: Either String String)

--- Integral ---
func113main = do
    print $ 123 `quot` 4
    print $ 123 `quot` (-4)

    print $ 123 `div` 4
    print $ 123 `div` (-4)

    print $ 123 `mod` 4
    print $ 123 `mod` (-4)

    print $ 123 `quotRem` 4
    print $ 123 `quotRem` (-4)

    print $ 123 `divMod` 4
    print $ 123 `divMod` (-4)

--- Data.IORef ---
--import Data.IORef
func114main = do
    ref <- newIORef 0
    value <- readIORef ref
    print value

    writeIORef ref $ value + 1
    readIORef ref >>= print

    modifyIORef ref (+ 2)
    readIORef ref >>= print

--- Transactions ---
--import Control.Concurrent.STM
type Account = CCSTM.TVar Integer
credit account amount = do
    current <- CCSTM.readTVar account
    CCSTM.writeTVar account (current + amount)
debit account amount = do
    current <- CCSTM.readTVar account
    CCSTM.writeTVar account (current - amount)
transfer from to amount =
    CCSTM.atomically $ do
        debit from amount
        credit to amount
--
func115main = do
    account1 <- CCSTM.atomically $ CCSTM.newTVar 10
    account2 <- CCSTM.atomically $ CCSTM.newTVar 20

    transfer account1 account2 5
    balance1 <- CCSTM.atomically $ CCSTM.readTVar account1
    balance2 <- CCSTM.atomically $ CCSTM.readTVar account2
    print balance1
    print balance2

--- Data.Tuple ---
--import Data.Tuple
func116main = print $ swap (1, 2)

--- Byte order ---
--import System.ByteOrder
func117main = print byteOrder

--- Byte dump ---
--import Text.Bytedump
func118main = do
    print $ TBD.hexString 100
    print $ TBD.dumpRawS "Hello, world!"
    print $ TBD.dumpS "Hello, world!"

--- UUID ---
--import Prelude hiding (null)
--import Data.UUID
--import Data.UUID.V1
--import Data.UUID.V3 as V3
--import Data.UUID.V4
--import Data.UUID.V5 as V5
func119main = do
    print nil
    print $ Data.UUID.null nil
    print $ toWords nil
    print $ fromWords 1 2 3 4
    print $ toString nil
    print $ fromString "00000000-0000-0000-0000-000000000000"

    uuid <- nextUUID
    print uuid

    random <- nextRandom
    print random

    print $ V3.namespaceDNS
    print $ V3.namespaceURL
    print $ V3.namespaceOID
    print $ V3.namespaceX500
    print $ V3.generateNamed V3.namespaceDNS [1, 2, 3]

    print $ V5.namespaceDNS
    print $ V5.namespaceURL
    print $ V5.namespaceOID
    print $ V5.namespaceX500
    print $ V5.generateNamed V5.namespaceDNS [1, 2, 3]

--- CPU information ---
--import System.Arch
--import System.Endian
func120main = do
    print $ getSystemArch
    print $ getSystemEndianness
    print $ toBE32 0xFF000000

--- HostName ---
--import Network.HostName
func121main = getHostName >>= print

--- SHA ---
--{-# LANGUAGE OverloadedStrings #-}
--import Data.Digest.Pure.SHA
--import Data.ByteString.Lazy.Char8
func122main = do
    print $ sha1 "Hello, world!"
    print $ sha224 "Hello, world!"
    print $ sha256 "Hello, world!"
    print $ sha384 "Hello, world!"
    print $ sha512 "Hello, world!"

    print $ hmacSha1 "key" "Hello, world!"
    print $ hmacSha224 "key" "Hello, world!"
    print $ hmacSha256 "key" "Hello, world!"
    print $ hmacSha384 "key" "Hello, world!"
    print $ hmacSha512 "key" "Hello, world!"

--- MD5 ---
--{-# LANGUAGE OverloadedStrings #-}
--import Data.Digest.Pure.MD5
--import Data.ByteString.Lazy.Char8
func123main = print $ md5 "Hello, world!"       -- 6cd3556deb0da54bca060b4c39479839

--- Punycode ---
-- N.B. punycode is not included yet in Stackage
--{-# LANGUAGE OverloadedStrings #-}
--import Data.Text.Punycode
--import Data.ByteString.Char8
--func124main = do
--    print $ PY.encode "Slovenský jazyk"
--    print $ PY.decode "Slovensk jazyk-2sb"

--- Dimensional ---
-- see stand_alone/Dimentional.hs
--import Numeric.Units.Dimensional.Prelude
--import qualified Prelude
{-
func126main = do
    print $ 1 *~  kilo meter
    print $ 1 *~ (kilo meter / hour)
    print $ 1 *~  newton
    print $ 1 *~  pascal
-}
{-
--- Java parser ---
- see stand_alone/JavaParser.hs
--{-# START_FILE main.hs #-}
--import Language.Java.Lexer
--import Language.Java.Parser
--import Language.Java.Pretty
func127main = do
    source <- readFile "Main.java"
    print $ lexer source
    print $ parser compilationUnit source

    let result = parser compilationUnit source
    case result of
        Left error -> print error
        Right ast -> putStrLn $ prettyPrint ast
-- {-# START_FILE Main.java #-}
-- public class Main {
--     public static void main(String[] args) {
--         System.out.println("Hello, world!");
--     }
-- }
--}

--- JavaScript parser ---
--{-# START_FILE main.hs #-}
--import Language.JavaScript.Parser
func128main = do
    source <- readFile "Main.js"
    print $ parse source "Main.js"

    let result = parse source "Main.js"
    case result of
        Left error -> print error
        Right ast -> putStrLn $ renderToString ast
-- {-# START_FILE Main.js #-}
-- function test() {
--    alert('Hello, world!');
-- }


--- Crypto.Hash ---
--{-# LANGUAGE OverloadedStrings #-}
{-
import Crypto.Hash.MD2 as MD2
import Crypto.Hash.MD4 as MD4
import Crypto.Hash.MD5 as MD5
import Crypto.Hash.RIPEMD160 as RIPEMD160
import Crypto.Hash.SHA1 as SHA1
import Crypto.Hash.SHA224 as SHA224
import Crypto.Hash.SHA256 as SHA256
import Crypto.Hash.SHA384 as SHA384
import Crypto.Hash.SHA512 as SHA512
import Crypto.Hash.Tiger as Tiger
import Crypto.Hash.Whirlpool as Whirlpool
import Data.ByteString.Base16
-}
func129main = do
    print $ BS16.encode $ MD2.hash "Hello, world!"
    print $ BS16.encode $ MD4.hash "Hello, world!"
    print $ BS16.encode $ MD5.hash "Hello, world!"
    print $ BS16.encode $ RIPEMD160.hash "Hello, world!"    
    print $ BS16.encode $ SHA1.hash "Hello, world!"
    print $ BS16.encode $ SHA224.hash "Hello, world!"
    print $ BS16.encode $ SHA256.hash "Hello, world!"
    print $ BS16.encode $ SHA384.hash "Hello, world!"
    print $ BS16.encode $ SHA512.hash "Hello, world!"
    print $ BS16.encode $ Tiger.hash "Hello, world!"
    print $ BS16.encode $ Whirlpool.hash "Hello, world!"

--- Diff ---
--{-# START_FILE main.hs #-}
--import Data.Algorithm.Diff
--import Data.Algorithm.DiffOutput
func130main = do
    file1 <- readFile "file1.txt"
    file2 <- readFile "file2.txt"
    let
        lines1 = lines file1
        lines2 = lines file2
    print $ getDiff lines1 lines2
    print $ getGroupedDiff lines1 lines2
    putStrLn $ ppDiff $ getGroupedDiff lines1 lines2
--{-# START_FILE file1.txt #-}
--first line
--second line
--{-# START_FILE file2.txt #-}
--first line
--hello
--third line

--- Zlib ---
--{-# LANGUAGE OverloadedStrings #-}
--import Codec.Compression.Zlib
--import Data.ByteString.Lazy.Char8
func131main = do
    print $ Codec.Compression.Zlib.compress "Hello, world!"
    print $ Codec.Compression.Zlib.decompress "x\156\243H\205\201\201\215Q(\207/\202IQ\EOT\NUL ^\EOT\138"

--- GZip ---
--{-# LANGUAGE OverloadedStrings #-}
--import Codec.Compression.GZip
--import Data.ByteString.Lazy.Char8
func132main = do
    print $ Codec.Compression.GZip.compress "Hello, world!"
    print $ Codec.Compression.GZip.decompress "\US\139\b\NUL\NUL\NUL\NUL\NUL\NUL\ETX\243H\205\201\201\215Q(\207/\202IQ\EOT\NUL\230\198\230\235\r\NUL\NUL\NUL"    

--- Linear Algebra ---
--import Linear
func133main = do
    print $ LN.V0

    print $ LN.V1 1
    print $ LN.V1 1 + LN.V1 2
    print $ LN.V1 1 - LN.V1 2
    print $ LN.V1 1 * LN.V1 2
    print $ LN.V1 1 / LN.V1 2

    print $ LN.V2 1 2
    print $ LN.V2 1 2 + LN.V2 3 4
    print $ LN.V2 1 2 - LN.V2 3 4
    print $ LN.V2 1 2 * LN.V2 3 4
    print $ LN.V2 1 2 / LN.V2 3 4
    print $ LN.perp $ LN.V2 0 1

    print $ LN.V3 1 2 3
    print $ LN.V3 1 2 3 + LN.V3 4 5 6
    print $ LN.V3 1 2 3 - LN.V3 4 5 6
    print $ LN.V3 1 2 3 * LN.V3 4 5 6
    print $ LN.V3 1 2 3 / LN.V3 4 5 6
    print $ LN.cross (LN.V3 1 2 3) (LN.V3 4 5 6)

    print $ LN.V4 1 2 3 4
    print $ LN.V4 1 2 3 4 + LN.V4 5 6 7 8
    print $ LN.V4 1 2 3 4 - LN.V4 5 6 7 8
    print $ LN.V4 1 2 3 4 * LN.V4 5 6 7 8
    print $ LN.V4 1 2 3 4 / LN.V4 5 6 7 8
    print $ LN.vector $ LN.V3 1 2 3
    print $ LN.point $ LN.V3 1 2 3

    print $ (LN.zero :: LN.V3 Double)
    print $ LN.negated $ LN.V3 1 2 3
    print $ LN.V3 1 2 3 LN.^* LN.V3 4 5 6
    print $ LN.V3 1 2 3 LN.*^ LN.V3 4 5 6
    print $ LN.V3 1 2 3 LN.^/ 2
    print $ LN.sumV [LN.V3 1 2 3, LN.V3 4 5 6, LN.V3 7 8 9]
    print $ (LN.basis :: [LN.V3 Int])
    print $ LN.basisFor $ LN.V3 1 2 3
    
    -- Variable not in scope: kronecker :: V3 Integer -> a0
    -- Perhaps you meant ‘NLA.kronecker’ (imported from Numeric.LinearAlgebra)
    --print $ NLA.kronecker $ V3 1 2 3      -- Kronecker product of two matrices.

    print $ LN.outer (LN.V3 1 2 3) (LN.V3 4 5 6)

    print $ LN.nearZero (1e-10 :: Double)
    print $ LN.nearZero (1e-15 :: Double)

    print $ LN.trace $ LN.V3 (LN.V3 1 2 3) (LN.V3 4 5 6) (LN.V3 7 8 9)
    print $ LN.diagonal $ LN.V3 (LN.V3 1 2 3) (LN.V3 4 5 6) (LN.V3 7 8 9)

    print $ LN.dot (LN.V3 1 2 3) (LN.V3 4 5 6)
    print $ LN.quadrance $ LN.V3 1 2 3
    print $ LN.qd (LN.V3 1 2 3) (LN.V3 4 5 6)
    print $ LN.distance (LN.V3 1 2 3) (LN.V3 4 5 6)
    print $ LN.norm $ LN.V3 1 2 3
    print $ LN.signorm $ LN.V3 1 2 3
    print $ LN.normalize (LN.V3 1 2 3 :: LN.V3 Double)

--- Data.Matrix ---
--import Data.Matrix
m1 = Data.Matrix.matrix 3 4 $ \(r, c) -> 4 * (r - 1) + c
m2 = Data.Matrix.fromList 3 4 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
m3 = Data.Matrix.fromLists [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]

func134main = do
    print m1
    print m2
    print m3

    print $ zero 3 4
    print $ identity 3
    print $ permMatrix 3 1 2

    print $ nrows m1
    print $ ncols m1

    print $ getElem 2 3 m1
    print $ m1 Data.Matrix.! (2, 3)
    print $ getRow 2 m1
    print $ getCol 3 m1
    print $ getDiag m1

    print $ setElem 13 (2, 3) m1
    print $ Data.Matrix.transpose m1
    print $ extendTo 0 4 8 m1
    print $ mapRow (\c x -> 2 * x) 3 m1

    print $ submatrix 2 3 1 2 m1
    print $ minorMatrix 1 2 m1
    print $ splitBlocks 2 3 m1

    print $ m1 Data.Matrix.<|> zero 3 2
    print $ m1 <-> zero 2 4

    print $ multStd m1 (identity 4)

    print $ scaleMatrix 2 m1
    print $ scaleRow 2 3 m1
    print $ combineRows 3 2 1 m1
    print $ switchRows 1 2 m1

    print $ luDecomp $ Data.Matrix.fromLists [[1.0, 2.0], [3.0, 4.0]]
    print $ Data.Matrix.trace m1
    print $ diagProd m1

    print $ detLaplace $ identity 3
    print $ detLU $ Data.Matrix.fromLists [[1.0, 2.0], [3.0, 4.0]]

    -- kronecker
    --print $ NLA.kronecker m4 m5
    --print $ NLA.kronecker m1 m2

--- Towers of Hanoi ---
data Tower = Tower1 | Tower2 | Tower3 deriving Show
move 0 from to temp = []
move n from to temp = move (n - 1) from temp to ++ [(from, to)] ++ move (n - 1) temp to from
func135main = print $ move 3 Tower1 Tower2 Tower3

--- C parser ---
--{-# START_FILE main.hs #-}
--import Language.C
func136main = do
    result <- parseCFilePre "test.c"
    case result of
        Left error -> print error
        Right ast -> do
            print ast
            print $ pretty ast
-- {-# START_FILE test.c #-}
-- It does not parse "//", "#", "/*" !!!
-- int main() {
--     printf("Hello, world!\n");
--     return 0;
-- }

--- Data.Either ---
--import Data.Either
--import Prelude hiding (error)
type ErrorOrValue = Either String Int
error = Left "MyError" :: ErrorOrValue
value = Right 123 :: ErrorOrValue
func137main = do
    print Lib5.error
    print value

    print $ isLeft Lib5.error
    print $ isLeft value

    print $ isRight Lib5.error
    print $ isRight value

    case Lib5.error of
        Left x -> print $ "Error: " ++ x
        Right x -> print $ "Value: " ++ show x

    case value of
        Left x -> print $ "Error: " ++ x
        Right x -> print $ "Value: " ++ show x

    print $ either show (show.(+ 1)) Lib5.error
    print $ either show (show.(+ 1)) value

    print $ either (\_ -> 0) id Lib5.error
    print $ either (\_ -> 0) id value

    print $ lefts [Lib5.error, value]
    print $ rights [Lib5.error, value]
    print $ partitionEithers [Lib5.error, value]

--- Data.Maybe ---
--import Data.Maybe
--import Data.Char
nothing = Nothing :: Maybe String
just = Just "Hello, world!" :: Maybe String
func138main = do
    print nothing
    print just

    print $ isNothing nothing
    print $ isNothing just

    print $ isJust nothing
    print $ isJust just

    case nothing of
        Nothing -> print "Nothing"
        Just x -> print x

    case just of
        Nothing -> print "Nothing"
        Just x -> print x

    print $ maybe "Default" (map toUpper) nothing
    print $ maybe "Default" (map toUpper) just

    print $ fromJust just

    print $ fromMaybe "Default" nothing
    print $ fromMaybe "Default" just

    print $ listToMaybe ([] :: [Int])
    print $ listToMaybe [1, 2, 3]

    print $ maybeToList nothing
    print $ maybeToList just

    print $ catMaybes [nothing, just]

    print $ mapMaybe (\_ -> (Nothing :: Maybe Int)) [1, 2, 3]
    print $ mapMaybe (\x -> Just x) [1, 2, 3]

--- Fibonacci versions: 1, 2, 3, 4 ---
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
func142main = print $ take 20 fibonacci
fib a b = a : fib b (a + b)
---
fibonacci' = fib 0 1
func139main = print $ take 20 fibonacci'
fib' n m a b
    | n == m = a
    | otherwise = fib' n (m + 1) b (a + b)
---
fibonacci'' n = fib' n 0 0 1
func140main = print [fibonacci'' n | n <- [0..19]]
fib'' 0 a b = a
fib'' n a b = fib'' (n - 1) b (a + b)
---
fibonacci''' n = fib'' n 0 1
func141main = print [fibonacci''' n | n <- [0..19]]

--- Coin changes ---
changeCount 0 _ = 1
changeCount _ [] = 0
changeCount n (coin : coins)
    | n > 0 = changeCount (n - coin) (coin : coins) + changeCount n coins
    | otherwise = 0
func143main = print $ changeCount 10 [8, 5, 1]

---
--import Data.List
--import Data.Ord
allChanges 0 _ = [[]]
allChanges _ [] = []
allChanges n (coin : coins)
    | n > 0 = map (coin :) (allChanges (n - coin) (coin : coins)) ++ allChanges n coins
    | otherwise = []

shortest = minimumBy $ comparing length
optimalChange n coins = shortest $ allChanges n coins

func144main = do
    print $ allChanges 10 [8, 5, 1]
    print $ optimalChange 10 [8, 5, 1]

---
bestChange 0 _ = Just []
bestChange _ [] = Nothing
bestChange n (coin : coins)
    | n > 0 = shorter (fmap (coin :) (bestChange (n - coin) (coin : coins))) (bestChange n coins)
    | otherwise = Nothing

shorter Nothing Nothing = Nothing
shorter (Just a) Nothing = Just a
shorter Nothing (Just b) = Just b
shorter (Just a) (Just b) = if length a < length b then Just a else Just b

func145main = print $ bestChange 10 [8, 5, 1]


--- Queens ---
queens n = queens' n n

queens' n 0 = [[]]
queens' n k = [x:xs | xs <- queens' n (k - 1), x <- [1..n], isSafeColumn x xs, isSafeDiagonal x xs]

isSafeColumn x xs = not $ elem x xs

isSafeDiagonal x xs = all (\(a, b) -> abs(x - a) /= b) $ zip xs [1..]

showLine n k = replicate (k - 1) '.' ++ "X" ++ replicate (n - k) '.'

showSolution s = (mapM_ putStrLn [showLine (length s) k | k <- s]) >> putStrLn ""

func146main = mapM_ showSolution $ queens 4


--- GHC options ---
--{-# OPTIONS_GHC -fwarn-missing-signatures #-}
--func147main = putStrLn "Hello, world!"

