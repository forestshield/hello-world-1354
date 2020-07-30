module Lib4
    ( someFuncLib4
    ) where

--import Prelude hiding (max, signum)
import Data.String
import Data.Int
--import GHC.Int
--import data-easy

someFuncLib4 :: IO ()
someFuncLib4 = do
  print "------- Working with Lists ------"
  putStrLn "putStrLn Lambda: λ someFuncLib4"      -- putStrLn Lambda: λ
  print awesome                                   -- ["Papuchon","curry",":)"]
  --awesome ++ also
  print sList1            -- ["Papuchon","curry",":)","Quake","The Simons"]
  print allAwesome        -- [["Papuchon","curry",":)"],["Quake","The Simons"]]
  print sList2            -- ["Papuchon","curry",":)","Quake","The Simons"], after concat
  print bVal
  putStrLn $ "length allAwesome = " ++ show (length allAwesome) -- 2
  putStrLn $ "length (concat allAwesome) = " ++ show (length (concat allAwesome))   -- 5
  putStrLn ".......isPaliandrome  'abcd' ..............." 
  print (isPalindrome "abcd")
  putStrLn ".......isPaliandrome2  'eabae' ..............." 
  print (isPalindrome2 "eabae")
  putStrLn ".......myAbs (-4) ..............." 
  print (myAbs (-4))
  putStrLn ".......myAbs2 5 ..............." 
  print (myAbs2 5)
  putStrLn ".......simpliest func with Lambda syntaxis .........." 
  putStrLn "id :: a -> a    -- type singnature"
  putStrLn "id x = x        -- regular definition"
  putStrLn "id = \\x -> x    -- lambda syntaxis definition"  -- prints id = \x -> x
  print $ id3 "xxx"                                 -- "xxx"
  print $ id3 5                                     -- 5  
  print $ id2 [1,2,4]                               -- [1,2,4]
  print $ id2 (5, "tuples")                         -- (5, "tuples")
  putStrLn "....... applyTwice .........." 
  print $ applyTwice id3 11                   -- 11
  print $ applyTwice add1 11                  -- 13
  print $ applyTwice reverse (reverse "abc")  -- "cba"
  print $ add 6 7                             -- 13
  putStrLn "....... applyThree .........." 
  print $ applyThree id3 11                   -- 11
  print $ applyThree add1 11                  -- 14
  print $ applyThree reverse "abc"            -- "cba"
  putStrLn "....... Algebraic Datatypes .........." 
  print c1    -- Car {company = "lexus", model = "RX350", year = 2014}
  print c2    -- Car {company = "lexus", model = "RX350", year = 2014}
  print bDif  -- c1 == c2 -- True
  putStrLn "c4 of Car2 cannot be compared or shown, \
  \because of it's constructor ?!"
  putStrLn "c4 of Car2: probably this is a bad/wrong constructor \
  \for Car2 (\"lexus\",  \"RX350\",  2014)"
  putStrLn "Car2: all others constructors are OK, c3, c5 and for Car \
  \c1 and c2 the same.    Car2 \"lexus\"  \"RX350\"  2014"
  putStrLn "  but c3 or c5 Car2 can: Car2 {company = \"lexus\", model = 'RX350', year = 2014}"
  print c3    -- Car {company = "lexus", model = "RX350", year = 2014}
  print c5    -- Car {company = "lexus", model = "RX350", year = 2014}
  print bDif2  -- c3 == c5 -- True
  putStrLn "----- Values, Functions and Types -----------"
  putStr $ show $ inc (square 5)    -- 26, a = inc (square 5)
  putStrLn ", inc (square 5)"
  putStr $ show $ square (inc 5)    -- 36, b = square (inc 5)
  putStrLn ", square (inc 5)"
  print "average (inc 3) (inc 5)  -- fails, wrong type of inc for average" 
  putStr $ show $ average (inc2 3) (inc2 5) -- 5,0 c = average (inc2 3) (inc2 5 
  putStrLn ", average (inc2 3) (inc2 5)"
  putStr $ show $ arithmetic_mean 9 11  
  putStrLn ", arithmetic_mean 9 11"         -- 10
  putStr $ show $ harmonic_mean 9 11  
  putStrLn ", harmonic_mean 9 11"           -- 9.9
  putStr $ show $ max' 9 11                   
  putStrLn ", max' 9 11"                    -- 11
  putStr $ show $ signum' (-11)                   
  putStrLn ", signum' (-11)"                -- 11
  putStrLn "-- Binders — Associating Names with Values or Functions"
  putStrLn "\n---------- Useful functions on lists ----------------"
  putStr $ show $ elem 3 lSample1                    
  putStrLn ", elem 3 is lSample1 [1, 2, 3, 4]"                      -- True
  putStr $ show $ sum [1, 2, 3, 4]                    
  putStrLn ", sum [1, 2, 3, 4]"                                     -- 10
  putStr $ show $ product [1, 2, 3, 4]                    
  putStrLn ", product [1, 2, 3, 4]"                                 -- 24
  putStr $ show $ maximum ['a', '0', 'C', 'd', 'e', 'z']                    
  putStrLn ", maximum ['a', '0', 'C', 'd', 'e', 'z']"               -- 'z'
  putStr $ show $ minimum ['a', '0', 'C', 'd', 'e', 'z']                    
  putStrLn ", minimum ['a', '0', 'C', 'd', 'e', 'z']"               -- 'C'
  putStr $ show $ maximum [1, 8, 3, (-4)]                    
  putStrLn ", maximum [1, 8, 3, (-4)]"                              -- 8
  putStr $ show $ minimum [1, 8, 3, (-4)]                    
  putStrLn ", minimum [1, 8, 3, (-4)]"                              -- -4
  putStrLn "lStr3 = elem 2 \"AbcdeFg\""                             -- failing to compile"
  putStrLn "lStr4   = maximum \"AbcdeFg\""                          -- failing to compile
  putStr $ show $ last ['a', '0', 'C', 'd', 'e', 'z']               -- Exception on empty list!     
  putStrLn ", last ['a', '0', 'C', 'd', 'e', 'z']"                  -- 'z'
  putStr $ show $ init ['a', '0', 'C', 'd', 'e', 'z']               -- Exception on empty list!     
  putStrLn ", init ['a', '0', 'C', 'd', 'e', 'z']"                  -- ['a', '0', 'C', 'd', 'e']
  putStr $ show $ null [1, 2, 3, 4]                    
  putStrLn ", null [1, 2, 3, 4]"                                     -- False
  putStr $ show $ null ("abc" :: [Char])
  putStrLn ", null' \"abc\" :: [Char]"                               -- False
  putStrLn "-- the last one does not compile without type specification for \"abc\" :: [Char]"
  putStr $ show $ null []                    
  putStrLn ", null []"                                               -- True
  putStr $ show $ take 24 [13, 26 .. ]                    
  putStrLn ", take 24 [13, 26 .. ]" -- [13,26,39,52,65,78,91,104,117,130,143,156,169,182,195,208,221,234,247,260,273,286,299,312]
  putStr $ show $ take 13 (cycle "abcdE")
  putStrLn ", take 13 (cycle \"abcdE\")"                             -- "abcdEabcdEabc"
  putStrLn "\n---- List Comprehension and iterate ------"
  putStr $ show $ [x*2 | x <- [1 .. 10]]                 
  putStrLn ", [x*2 | x <- [1 .. 10]]"                                -- [2,4,6,8,10,12,14,16,18,20]
  putStr $ show $ [x*2 | x <- [1..10], x*2 >= 12]
  putStrLn ", [x*2 | x <- [1..10], x*2 >= 12]"                       -- [12,14,16,18,20]
  putStr $ show $ take 10 (iterate (2*)1)             
  putStrLn ", take 10 (iterate (2*)1)"                 -- [1,2,4,8,16,32,64,128,256,512]
  putStr $ show $ take 10 (iterate (\x -> (x+3)*2)1)         
  putStrLn ", take 10 (iterate (\\x -> (x+3) * 2) 1)"  -- [1,8,22,50,106,218,442,890,1786,3578]
  putStrLn "---- list replicate, repeat ------"
  putStr $ show $ replicate 3 5                    
  putStrLn ", replicate 3 5"                           -- [5,5,5]
  putStr $ show $ replicate 3 'a'                    
  putStrLn ", replicate 3 'a'"                         -- "aaa"
  putStr $ show $ replicate 3 "a"                    
  putStrLn ", replicate 3 \"a\""                       -- ["a","a","a"]
  putStr $ show $ take 10 $ repeat 5                    
  putStrLn ", take 10 repeat 5"                        -- [5,5,5,5,5,5,5,5,5,5]
  putStrLn "---- list cycle ------"
  putStr $ show $ take 12 (cycle "LOL ")                    
  putStrLn ", take 12 (cycle \"LOL \")"                -- "LOL LOL LOL "
  putStr $ show $ take 10 (cycle [1,2,3])                    
  putStrLn ", take 10 (cycle [1,2,3])"                 -- [1,2,3,1,2,3,1,2,3,1]
  putStrLn "--- All numbers from 50 to 100 whose remainder when divided with the number 7 is 3"
  putStr $ show $ [ x | x <- [50..100], x `mod` 7 == 3]  -- [52,59,66,73,80,87,94]
  putStrLn ", [ x | x <- [50..100], x `mod` 7 == 3]"   -- [52,59,66,73,80,87,94]
  putStrLn " -- All numbers from 10 to 20 that are not 13, 15 or 19"
  putStr $ show $ [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]  
  putStrLn ", [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]"  -- [10,11,12,14,16,17,18,20]  
  putStrLn "----- All the possible combinations between numbers in two lists ------"
  putStr $ show $ [ x*y | x <- [2,5,10], y <- [8,10,11]]  
  putStrLn ", [ x*y | x <- [2,5,10], y <- [8,10,11]]"   -- [16,20,22,40,50,55,80,100,110] 
  putStrLn "\n --This function replaces every element of a list with 1 and then sums that up"
  putStr $ show $ length' "a2b4c6"
  putStrLn ", length' xs = sum [1 | _ <- xs],  length' \"a2b4c6\""    -- 6

  putStrLn "\n--Function that takes a string and removes everything except uppercase letters from it"
  putStr $ show $ removeNonUppercase "EveryThing is GooD!"
  putStrLn ", removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']],   \"EveryThing is GooD!\""

  putStrLn "\n-- Remove all odd numbers without flattening the list."
  putStrLn "xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]"  
  putStr $ show $ [ [ x | x <- xs, even x ] | xs <- xxs] 
  putStrLn ", [ [ x | x <- xs, even x ] | xs <- xxs]"     -- [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]] 

  putStrLn "\n---- Error Customization ----"
  --putStr $ head' []       -- Prelude.head: empty list
                          --      CallStack (from HasCallStack):
  --    error, called at src/Lib4.hs:507:12 in L4-0.1.0.0-Loc4iOrOwXG4zsh11hyC4Z:Lib4
  --putStr $ head []        -- Prelude.head: empty list
  putStrLn "----  ----"
  putStr $ show $ minimum [1, 8, 3, (-4)]                    
  putStrLn ", minimum [1, 8, 3, (-4)]"                      -- -4  
  putStr $ show $ mangle "Hello"                              
  putStrLn ", mangle \"Hello\""                             -- "elloH"
  putStr $ show $ mangle ""
  putStrLn ", mangle \"\""                                  -- ""
  putStr $ show $ mangle "I"
  putStrLn ", mangle \"I\""                                 -- "I"
  putStrLn "---------- zip function, creates list of tuples ---------" 
  putStr $ show $ zip [1 .. 5] ["one", "two", "three", "four", "five"]
  putStrLn ", zip [1 .. 5] [\"one\", \"two\", \"three\", \"four\", \"five\"]"
                                    -- [(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]
  putStr $ show $ zip [1,2,3,4,5] [5,5,5,5,5]
  putStrLn ", zip [1,2,3,4,5] [5,5,5,5,5]"                  -- [(1,5),(2,5),(3,5),(4,5),(5,5)] 
  putStrLn "---------- right triangles ---------" 
  putStrLn "triangles      = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]" 
  putStrLn "rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2"
  putStr "rightTriangles = " 
  putStrLn $ show $ rightTriangles   -- [(3,4,5),(6,8,10)]

  putStrLn "\n----------- compare -------------------------"
  putStr $ show $ "Abrakadabra" `compare` "Zebra"
  putStrLn ",  \"Abrakadabra\" `compare` \"Zebra\"" -- LT
  ---
  putStr $ show $ "Abrakadabra" < "Zebra"
  putStrLn ", \"Abrakadabra\" < \"Zebra\""          -- True
  ---
  putStr $ show $ 5 `compare` 3 
  putStrLn ", 5 `compare` 3"                        -- GT
  putStr $ show $ 5 `compare` 5
  putStrLn ", 5 `compare` 5"                        -- EQ

  putStrLn "\n------------- read ----------------------------"
  putStr $ show $ read "True" || False
  putStrLn ", read \"True\" || False"               -- True
  putStr $ show $ read "8.2" + 3.8        
  putStrLn ", read \"8.2\" + 3.8"                   -- 12.0
  putStr $ show $ read "[1,2,3,4]" ++ [99]
  putStrLn ", read \"[1,2,3,4]\" ++ [99]"           -- [1,2,3,4,99]
  putStrLn "\n------- read as ..."
  putStr $ show (read "5" :: Int)
  putStrLn ", read \"5\" :: Int"                    -- 5 
  putStr $ show (read "5" :: Float)
  putStrLn ", read \"5\" :: Float"                  -- 5.0
  putStr $ show ((read "5" :: Float) * 4)
  putStrLn ", (read \"5\" :: Float) * 4"            -- 20.0
  putStr $ show (read "[1,2,3,4]" :: [Int])
  putStrLn ", read \"[1,2,3,4]\" :: [Int]"          -- [1,2,3,4]
  putStr $ show (read "(3, 'a')" :: (Int, Char))
  putStrLn ", \"(3, 'a')\" :: (Int, Char)"          -- (3, 'a')
  putStrLn "\n------------- enum, funcs pred and succ ---------------------"
  putStrLn "Types in this class: (), Bool, Char, Ordering, Int, Integer, Float and Double"
  putStr $ show $ ['a'..'e']  
  putStrLn ", ['a'..'e']"       -- "abcde"  
  putStr $ show $ [LT .. GT]  
  putStrLn ", [LT .. GT]"       -- [LT,EQ,GT]
  putStr $ show $ [3 .. 5]    
  putStrLn ", [3 .. 5] "        -- [3,4,5]  
  putStr $ show $ succ 'B'    
  putStrLn ", succ 'B' "        -- 'C'
  putStr $ show $ pred 'B'    
  putStrLn ", pred 'B'"         -- 'A'

  putStrLn "\n------------- Bounded ---------------------" 
  putStr $ show (minBound :: Int)     -- -9223372036854775808
  putStrLn ", (minBound :: Int)"
  putStr $ show (maxBound :: Int)     -- 9223372036854775807
  putStrLn ", (maxBound :: Int)"
  putStr $ show (minBound :: Char)    -- '\NUL'
  putStrLn ", (minBound :: Char)"  
  putStr $ show $ succ (minBound :: Char)    -- '\SOH'
  putStrLn ", succ (minBound :: Char)"  
  putStr $ show $ pred (maxBound :: Char)    -- '\1114110'
  putStrLn ", pred (maxBound :: Char)"  
  putStr $ show (maxBound :: Char)    -- '\1114111'  
  putStrLn ", (maxBound :: Char)"  
  putStr $ show (maxBound :: Bool)    -- True  
  putStrLn ", (maxBound :: Bool)"  
  putStr $ show (minBound :: Int8)    -- -128
  putStrLn ", (minBound :: Int8)"  
  putStr $ show (maxBound :: Int32)    -- 2147483647
  putStrLn ", (maxBound :: Int32)"  
  --putStrLn "\n------------- fromIntegral ---------------------" 
  --putStr $ show $ fromIntegral (minBound :: Int16) + 3.2       -- -32764.8
  --putStrLn ", fromIntegral (minBound :: Int16) + 3.2"
  --putStrLn "But, (minBound :: Int16) + 3.2 -- compiler error"
  specShow (fromIntegral (minBound :: Int16) + 3.2)
           ", fromIntegral (minBound :: Int16) + 3.2 \n\
           \But, (minBound :: Int16) + 3.2 -- compiler error"
           "fromIntegral"
  specShow ((first (1,2,3)), (second (1,2,3)), (third (1,2,3)))
           ",   ((first (1,2,3)), (second (1,2,3)), (third (1,2,3)))\n\
           \first (x, _, _) = x\nsecond (_, y, _) = y\nthird (_, _, z) = z"           
           "Tuples Third"
  specShow ((sayMe 1), (sayMe 2), (sayMe 3), (sayMe 99))
           ",\n ((sayMe 1 = \"One!\"), (sayMe 2 = \"Two!\"), (sayMe 3 = \"Three!\"), (sayMe x = \"Not between 1 and 3\"))\n"
           "Pattern Matching with \"Catch all case\""
  specShow (addVectors (1, 2) (2, 3))
           ", addVectors (1, 2) (2, 3)\n\
           \addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)"
           "Pattern Matching in addVectors"
  specShow [a+b | (a,b) <- [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]]
           ", [a+b | (a,b) <- [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]]\n"
           "Pattern Matching in List Comprehension"
           
  specShow (({-head' [] :: Int-}), (head' "Hello"))
           ", ({-head' [] :: Int-}), (head' \"Hello\")\n\
           \N.B. head' on empty list commented, Exception"
           "Pattern Matching & customization of the error"
           
  specShow (tell [1], tell[1,2], tell[1,2,3])
           ", \n\
           \"
           "Pattern Matching tell function"

  --tell [1]
  --putStr $ show $ length "a"
  --putStrLn $ show (length "a") 
  putStrLn $ show $ length' "a"
  putStrLn $ show $ length'' "a2b4c6"
  putStrLn $ show $ length' ""
  --putStrLn $ show $ length "abc" :: IO ()
  --putStrLn $ show (length "a") -- does not work, if "- OverloadedStrings" is on 
  --(tell [])

  specShow ('\0')
           ", \n"           
           "Test"
{-
  specShow ()
           ", \n\
           \"
           ""
-}
-- specShow --------------------------------
specShow :: Show a => a -> String -> String -> IO ()
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
specHeader  a | a /= "" = specHeader2 a


-- working with Lists
par1 = "Papuchon"
awesome = [par1, "curry", ":)"]
sL41 = "The Simons"
also = ["Quake", sL41]
allAwesome = [awesome, also]
sList1 = awesome ++ also
sList2 = concat allAwesome  -- changes List from 2 nested Lists to List of 5 strings
bVal = sList1 == sList2

-- isPalindrome
isPalindrome :: (Eq a) => [a] -> Bool
--isPalindrome x = undefined
isPalindrome x = do  
  let y = reverse x
  x == y

-- isPalindrome2 better one
isPalindrome2 :: (Eq a) => [a] -> Bool
isPalindrome2 x = reverse x == x
  
-- returning abs value
-- myAbs better one
myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else negate x

-- myAbs2
myAbs2 :: Integer -> Integer
myAbs2 x = do
  if x > 0 
    then x 
  else 
    negate x

-- simpliest function, regular definition
id3 :: a -> a
id3 x = x

-- simpliest function, lambda sysntaxis
id2 :: a -> a
id2  = \x -> x

-- applyTwice 
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- applyThree
applyThree :: (a -> a) -> a -> a
applyThree f x = f (f (f x))

-- add
add :: Num a => a -> a -> a 
add x y = x + y

-- add1
arg1 = 3 :: Int
arg2 = 5 :: Int
--
add1 :: Int -> Int
add1 x = x + (1 :: Int)
--add1 = add 1

-- addTupl
addTupl :: (Int, Int) -> Int
--addTupl :: Num a => (a, a) -> a
addTupl (a, b) = add a b

-- curry and uncurry
curry'        :: ((a, b) -> c) -> a -> b -> c
curry' f x y  =  f (x, y)
uncurry'      :: (a -> b -> c) -> (a, b) -> c
uncurry' f p  =  f (fst p) (snd p)

-- uncurryAdd
uncurryAdd :: (Int, Int) -> Int
uncurryAdd = uncurry add
--
example :: Int
example = uncurryAdd (1, 2)

curryAddTupl :: ((Int, Int) -> Int) -> Int -> Int -> Int
curryAddTupl = undefined
--curryAddTupl = curry addTupl


-- Algebraic Datatypes ----------
-- see Cards.hs too
-- data Car
data Car = Car  { company :: String
                , model :: String 
                , year :: Int               
                } deriving (Show, Read, Eq)
c1   = Car "lexus"  "RX350"  2014
c2   = Car "lexus"  "RX350"  2014
bDif = c1 == c2 

-- deriving does not work here, because unknown type !!!
data Car2 a b c = Car2  { company2 :: a
                        , model2 :: b 
                        , year2 :: c               
                        } deriving (Show, Read, Eq)        
c3   = Car2 "lexus"  "RX350"  2014
c4   = Car2 ("lexus",  "RX350",  2014)  -- this constractor does not work really
c5   = Car2 "lexus"  "RX350"  2014      -- this is OK
--c3 == c4, fails, no Eq ???
--bDif2 = c3 == c4    -- fails, no Eq, because of unknown type !!!
bDif2 = c3 == c5    -- True

-- data Color                               
data Color  = Blue | Green | Read deriving (Show, Read, Eq) 
--data Color2 = funcRGB Int Int Int 

-- func RGB
--funcRGB :: Int -> Int -> Int -> Color 
--funcRGB a b c =  

---------------------------------------------
-- data SBTree (String)
data SBTree = Leaf String
            | Branch String SBTree SBTree deriving (Show, Eq, Read)
-- data BBTree (Bool)
data BBTree = Leaf2 Bool
            | Branch2 Bool BBTree BBTree 
-- data BTree (universal)
data BTree a = Leaf3 a
             | Branch3 a (BTree a) (BTree a)
{-
Step back here a moment and take note of the similarities.
A data constructor is a "function" that takes 0 or more values and gives you back a new value.
A type constructor is a "function" that takes 0 or more types and gives you back a new type.
Data constructors with parameters are cool if we want slight variations 
in our values – we put those variations in parameters and let the guy 
who creates the value decide what arguments they are going to put in. 
In the same sense, type constructors with parameters are cool if we want 
slight variations in our types! We put those variations as parameters and 
let the guy who creates the type decide what arguments they are going to put in.
-}             
sbL1    = Leaf "Leaf1"
sbB1    = Branch "Branch1"
sbL2    = Leaf "Leaf2"
sbB2    = Branch "Branch2"
sbL3    = Leaf "Leaf3"
sbB3    = Branch "Branch3"

--sbTree1 = SBTree sbL1  
--sbTree2 = SBTree sbB1 
--SBTree :: SBTree -> sbt SBTree
-------------------------------
{-
{-
-- data Point
data Point  = Point Int Int deriving (Eq, Show, Read)
data Point2 = Point2 { x :: Int, y :: Int} deriving Show
-- addPoint
addPoint          :: (Point, Point) -> Point
--addPoint (p1, p2) = p1 + p2 
--(+) :: Point -> Point -> Point
-}
{-
data Point      = Pt Float Float
pointx          :: Point -> Float
pointx (Pt x _) = x
pointy          :: Point -> Float
pointy (Pt y _) = y
-}

-- data Point
data Point1       = Pt1 {pointx1, pointy1 :: Float} deriving (Show, Eq, Read)

-- data Point2
data Point2       = Pt2 Float Float deriving (Show, Eq, Read)
pointx2           :: Point2 -> Float
pointx2 (Pt2 x _) = x
pointy2           :: Point2 -> Float
pointy2 (Pt2 y _) = y

myPoint1 = ((0, 0), (1, 1))
myPoint2 = ((3, 3), (5, 5))

myPoint3 = ((0.0, 1.0), (2.0, 3.0))
myPoint4 = ((4.0, 5.0), (6.0, 7.0))

-- func absPoint
absPoint        :: Point1 -> Float
absPoint p      =  sqrt (pointx1 p * pointx1 p + 
                         pointy1 p * pointy1 p)

--addPoint   :: Point -> Point -> Float
--addPoint   = (pointx x1 + pointx x2, pointy y1 + pointy y2)

-- data Shape
--data Shape
-}

----- Values, Functions and Types -----------
-- http://learn.hfm.io/first_steps.html
inc   :: Int -> Int
inc x = x + 1 

-- any type of Num inc2
inc2   :: Num a => a -> a 
inc2 x = x + 1

average :: Float -> Float -> Float
average a b  = (a + b) / 2.0

-- Signatures
--show :: Show a => a -> String
--(==), (/=) :: Eq a => a -> a -> Bool
--(<), (>), (<=), (>=) :: Ord a => a -> a-> Bool
--(+), (-), (*) :: Num a => a -> a -> a
--div, mod :: Integral a => a -> a -> a
--(/) :: Fractional a => a -> a -> a
--sin, cos, tan, exp, sqrt,… :: Floating a => a -> a

{-
Typeclass Show
functions: show :: Show a => a -> String: convert the given value into a string.
member types: almost all predefined types, excluding function types.

Typeclass Eq
functions: (==), (/=) :: Eq a => a -> a -> Bool: equality and inequality.
member types: almost all predefined types, excluding function types.

Typeclass Ord
functions: (<), (>), (<=), (>=) :: Ord a => a -> a-> Bool: less than, greater than, 
less or equal, greater or equal
member types: almost all predefined types, excluding function types.

all types in Ord are already in Eq, so if you are using both == and < on a value, 
  it is sufficient to require it to be in Ord.

Typeclass Num
functions: (+), (-), (*) :: Num a => a -> a -> a: arithmetic operations.
member types: Float, Double, Int, Integer

Typeclass Integral
functions: div, mod :: Integral a => a -> a -> a: division.

member types: Int (fixed precision), Integer (arbitrary precision)
Typeclass Fractional

functions: (/) :: Fractional a => a -> a -> a: division.
member types: Float, Double

Typeclass Floating
functions: sin, cos, tan, exp, sqrt,… :: Floating a => a -> a: trigonometric and 
other functions.

member types: Float, Double
We will introduce more type classes and operations as we use them. 
If you want to find out more about a type class, select its name and type ⌘-i 
in Haskell for Mac, or use :info TYPECLASS-NAME in GHCi.
-}

--square
square :: Int -> Int
square x = x * x

--tests
a = inc (square 5)
b = square (inc 5)
--d = average (inc 3) (inc 5) -- fails
c = average (inc2 3) (inc2 5) -- works

--------------
-- calculates the arithmetic mean of two numbers
arithmetic_mean :: Fractional a => a -> a -> a
arithmetic_mean x y  = (x + y)  / 2

-- calculates the harmonic mean of two numbers
harmonic_mean :: Fractional a => a -> a -> a
harmonic_mean x y  = 2 * x * y / (x + y)

-- Branches and Control Flow
max'     :: Ord a => a -> a -> a
--max' x y = if x >= y then x else y
-- better max
max' x y | x >= y    = x 
         | otherwise = y

signum' :: (Ord a, Num a) => a -> Int
--signum' x = if x < 0 then -1 else if x == 0 then 0 else 1
--better to read
--signum' x | x <  0  = -1
--          | x == 0  = 0
--          | x >  0  = 1
-- even better
signum' x | x <  0     = -1
          | x == 0     = 0
          | otherwise  = 1

-- Binders — Associating Names with Values or Functions
pi' :: Floating a => a
pi' = 3.141592653589793

--circleArea
circleArea          :: Floating a => a -> a
circleArea diameter = pi * radius * radius
  where
    radius = diameter / 2.0       -- local binding

-- Point, using Tuples
type Point = (Int, Int)
--
origin' :: Point
origin' = (0, 0)
-- move a given point to the right
moveRight :: Point -> Int -> Point
moveRight (x, y) distance'  = (x + distance', y)
-- move a given point to upwards
moveUp :: Point -> Int -> Point
moveUp (x, y) distance'  = (x, y + distance')

-- Color
type Colour = String
-- new name for the type of colour points
type ColourPoint = (Int, Int, Colour)
-- origin of the coordinate system in a given colour
--
origin         :: Colour -> ColourPoint
origin colour  = (0, 0, colour)

-- move a colour point vertically and horizontally
move :: ColourPoint -> Int -> Int -> ColourPoint
move (x, y, colour) xDistance yDistance  
  = (x + xDistance, y + yDistance, colour)
-- compute the distance between two colour points
distance :: ColourPoint -> ColourPoint -> Float
distance (x1, y1, colour1) (x2, y2, colour2) 
  = sqrt (fromIntegral (dx * dx + dy * dy))
  where
    dx = x2 - x1
    dy = y2 - y1
-- The standard function fromIntegral converts any integral type to any other numeric type. 
-- Its signature is
--fromIntegral :: (Integral a, Num b) => a -> b

startPoint = (0, 0, "black")
colourOfPoint (x, y, colour) = colour

-- Special names for some tuples.
-- The following table lists a number of tuple types and their names:
-- #	 	Expression	 	Name
-- 0		()		                      Unit
-- 1		n/a		                      n/a
-- 2		(x_1, x_2)		              Pair
-- 3		(x_1, x_2, x_3)		          Triple
-- 4		(x_1, x_2, x_3, x_4)		    Quadruple
-- 5		(x_1, x_2, x_3, x_4, x_5)		Quintuple
-- ...
-- n		(x_1, …, x_n)		            n-tuple

-- Lists: Many Values of a Single Type ---------------
firstTenPrimes :: [Int]
firstTenPrimes  = [2, 3, 5, 7, 11, 13, 17, 19, 23, 27]

oneToTwenty :: [Int]
oneToTwenty = [1..20]

-- return all positive odd numbers up to maxNumber
oddNumbers :: Int -> [Int]
oddNumbers maxNumber  = [1, 3..maxNumber]

--Compare Tuple and List ------------------------
tSample1 = (1, 2, "green") :: (Int, Int, String)
-- vs
lSample1  = [1, 2, 3, 4] :: [Int]

-- Usefull functions on List
-- (:) :: a -> [a] -> [a]       -- cons operator
-- (++) :: [a] -> [a] -> [a]    -- concat operator
-- (!!) :: [a] -> Int -> a      -- index operator -- Exception on empty list or too big/small index
-- head :: [a] -> a             -- head -- Exception on empty list 
-- tail :: [a] -> [a]           -- tail -- Exception on empty list 
-- drop :: Int -> [a] -> [a]    -- droping first a chars, no Exception
-- take :: Int -> [a] -> [a]    -- taking first a chars, no Exception
-- last :: [a] -> a             -- last -- Exception on empty list 
-- init :: [a] -> [a]           -- init -- Exception on empty list 
-- reverse :: [a] -> [a]        -- reversing string
-- length :: [a] -> Int         -- length 
-- null :: [a] -> Bool          -- is list empty
-- maximum :: Ord a => [a] -> a
-- minimum :: Ord a => [a] -> a
-- sum     :: Num a => [a] -> a
-- product :: Num a => [a] -> a
-- Check if an item is contained in a list
-- elem :: Eq a => a -> [a] -> Bool  
-- zip. It takes two lists and then zips them together into one list by joining the matching 
--      elements into pairs. It's a really simple function but it has loads of uses. 
--      It's especially useful for when you want to combine two lists in a way or traverse 
--      two lists simultaneously. Here's a demonstration. 
-- zip :: [a] -> [b] -> [(a, b)]
-- cycle takes a list and cycles it into an infinite list. 
--      If you just try to display the result, it will go on forever 
--      so you have to slice it off somewhere. 
-- cycle  :: [a] -> [a]        -- Exception on empty list
-- repeat takes an element and produces an infinite list of just that element. 
--      It's like cycling a list with only one element.
-- repeat :: a -> [a]
-- replicate function if you want some number of the same element in a list. 
--      replicate 3 10 returns [10,10,10]
-- replicate :: Int -> a -> [a]
-- iterate creates an infinite list where the first item is calculated by applying 
--      the function on the secod argument, the second item by applying the function 
--      on the previous result and so on.
-- iterate :: (a->a)->a->[a]
-----
lS2     = elem 2 lSample1 
lS3     = sum [1, 2, 3, 4]
lS4     = product [1, 2, 3, 4] 
lS5     = maximum [1, 8, 3, (-4)]
lS6     = minimum [1, 8, 3, (-4)] 
lStr1   = maximum ['a', 'b', 'C', 'd', 'e', 'z']
lStr2   = minimum ['a', 'b', 'C', 'd', 'e', 'z']
--lStr3   = elem 2 "AbcdeFg"  -- failing to compile
--lStr4   = maximum "AbcdeFg"  -- failing to compile
--concat
exclaim :: String -> String
exclaim sentence  = sentence ++ "!"

listFromZip1 = zip [1 .. 5] ["one", "two", "three", "four", "five"] 
listFromZip2 = zip [1,2,3,4,5] [5,5,5,5,5]
--[(1,5),(2,5),(3,5),(4,5),(5,5)]
--[(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")] 

-- null function ---------
null'       :: [a] -> Bool 
null' []    = True 
null' (_:_) = False 

------ String --------- Partial Functions ---------
--type String = [Char]
-- more for drop and take see Print3flipped.hs
--
--head :: [a] -> a
--head (x:xs) = x
-- The function head is defined by pattern matching using the same symmetry 
-- between list construction and list pattern matching as we discussed 
-- previously for tuples — i.e., it matches on the cons-operator (:):

-- Customization of error and Pattern matching ---
--error :: String -> a
head' :: [a] -> a
head' [] = error "Prelude.head: empty list"
head' (x:_) = x  


-- List Comprehension -----------------------------------------------------
lList2 = [x*2 | x <- [1 .. 10]]             -- [2,4,6,8,10,12,14,16,18,20]
lList3 = [x*2 | x <- [1..10], x*2 >= 12]    -- [12,14,16,18,20]

-- iterare
lList4 = take 10 (iterate (2*)1)             -- [1,2,4,8,16,32,64,128,256,512]
lList5 = take 10 (iterate (\x -> (x+3)*2)1)  -- [1,8,22,50,106,218,442,890,1786,3578]

-- cycle
lList6 = take 10 (cycle [1,2,3]) -- [1,2,3,1,2,3,1,2,3,1]  
lList7 = take 12 (cycle "LOL ")  -- "LOL LOL LOL "   

-- repeat
lList8 = take 10 (repeat 5)      -- [5,5,5,5,5,5,5,5,5,5]  

-- replicate
lList9 = replicate 3 5          -- [5,5,5]  
lList10 = replicate 3 'a'       -- "aaa"
lList11 = replicate 3 "a"       -- ['a','a','a']  

-- All numbers from 50 to 100 whose remainder when divided with the number 7 is 3
lList12 = [ x | x <- [50..100], x `mod` 7 == 3]  -- [52,59,66,73,80,87,94] 
lList13 = [ x | x <- [50..100], mod x 7 == 3]    -- [52,59,66,73,80,87,94] 
 
-- List filtering
--    A comprehension th at replaces each odd number greater than 10 with "BANG!" 
--    and each odd number that's less than 10 with "BOOM!". If a number isn't odd, 
--    we throw it out of our list
--strBoom :: [Char] 
--strBoom = "BOOM!"
--strBang :: [Char]
--strBang = "BANG!"
--a2 :: [Char]

-- does not work, if "- OverloadedStrings" is off ?!
boomBangs :: (Integral a1, Data.String.IsString a2) => [a1] -> [a2]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
--boomBangs xs = [ if x < 10 then strBoom else strBoom | x <- xs, odd x]
--oddEven
oddEven :: (Integral a1, Data.String.IsString a2) => [a1] -> [a2]
oddEven xs = [if even x then "even" else "odd" | x <- xs]

--lList14   -- All numbers from 10 to 20 that are not 13, 15 or 19
lList14 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]  -- [10,11,12,14,16,17,18,20]  

-- If we have two lists, [2,5,10] and [8,10,11] and we want to get the products of 
--    all the possible combinations between numbers in those lists
lList15 = [ x*y | x <- [2,5,10], y <- [8,10,11]]  -- [16,20,22,40,50,55,80,100,110]   

-- Function that takes a string and removes everything except uppercase letters from it.
removeNonUppercase    :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Remove all odd numbers without flattening the list.
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
lList16 = [ [ x | x <- xs, even x ] | xs <- xxs]  
-- [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]  
-----------------------------
-- mangle
mangle :: String -> String
mangle [] = ""
mangle x = tail x ++ take 1 x

-----------------------------
-- Here's a problem that combines tuples and list comprehensions: 
-- which right triangle that has integers for all sides and all sides equal to 
-- or smaller than 10 has a perimeter of 24? First, let's try generating 
-- all triangles with sides equal to or smaller than 10:
triangles :: [(Integer, Integer, Integer)]
triangles      = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]  
---
rightTriangles :: [(Integer, Integer, Integer)]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]   

-- factorial
factorial :: Integer -> Integer  
factorial n = product [1..n] 

-- circumference
circumference :: Float -> Float       -- circumference 4 = 25.132742
circumference r = 2 * pi * r  
---
circumference' :: Double -> Double  
circumference' r = 2 * pi * r         -- circumference' 4 = 25.132741228718345

-- compare -------------------------
-- (>) :: (Ord a) => a -> a -> Bool 
resCompare1 = "Abrakadabra" `compare` "Zebra"     -- LT
resCompare2 = "Abrakadabra" < "Zebra"             -- True
resCompare3 = 5 `compare` 3                       -- GT
resCompare4 = 5 `compare` 5                       -- EQ

-- read ----------------------------
-- read :: (Read a) => String -> a
resRead1 = read "True" || False         -- True
resRead2 = read "8.2" + 3.8             -- 12.0
resRead3 = read "[1,2,3,4]" ++ [99]     -- [1,2,3,4,99]
resRead4 = read "5" :: Int              -- 5 
resRead5 = read "5" :: Float            -- 5.0
resRead6 = (read "5" :: Float) * 4      -- 20.0
resRead7 = read "[1,2,3,4]" :: [Int]    -- [1,2,3,4]
resRead8 = read "(3, 'a')" :: (Int, Char)  -- (3, 'a')

-- enum ----------------------------
--    Enum members are sequentially ordered types — they can be enumerated. '
--    The main advantage of the Enum typeclass is that we can use its types in list ranges. 
--    They also have defined successors and predecesors, which you can get with 
--    the succ and pred functions. 
--    Types in this class: (), Bool, Char, Ordering, Int, Integer, Float and Double.
resEnum1 = ['a'..'e']  -- "abcde"  
resEnum2 = [LT .. GT]  -- [LT,EQ,GT]  
resEnum3 = [3 .. 5]    -- [3,4,5]  
resEnum4 = succ 'B'    -- 'C'
resEnum5 = pred 'B'    -- 'A'
-- Bounded ------------------------
--     Bounded members have an upper and a lower bound.
-- maxBound :: (Bool, Int, Char)
-- minBound :: (Bool, Int, Char)
resBound1  = minBound :: Int     -- -9223372036854775808    \= -2147483648
resBound2  = maxBound :: Int     -- 9223372036854775807
resBound3  = minBound :: Char    -- '\NUL'
resBound3' = succ (minBound :: Char)    -- '\SOH'
--resBound3'' = pred (minBound :: Char)    -- Exception
resBound4  = maxBound :: Char    -- '\1114111'  
resBound5  = maxBound :: Bool    -- True  
resBound6  = minBound :: Bool    -- False  
resBound7  = minBound :: Int8    -- -128
resBound8  = maxBound :: Int8    -- 127
resBound9  = minBound :: Int16    -- -32768
resBound10 = maxBound :: Int16    -- 32767
resBound11 = minBound :: Int32    -- -2147483648
resBound12 = maxBound :: Int32    -- 2147483647
resBound13 = minBound :: Int64    -- -9223372036854775808
resBound14 = maxBound :: Int64    -- 9223372036854775807

resFromInt = fromIntegral (minBound :: Int16) + 3.2
-- resJust    = (minBound :: Int16) + 3.2 -- compiler error

-- fromIntegral -------------------
--    it takes an integral number and turns it into a more general number. 
-- fromIntegral :: (Num b, Integral a) => a -> b

-- Pattern matching ---------------
-- factorial2
factorial2 :: (Integral a) => a -> a  
factorial2 0 = 1  
factorial2 n = n * factorial2 (n - 1)     -- recursion
-- sayMe
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
--sayMe 4 = "Four!"  
--sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 3" 
-- charName
charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"
-- lucky
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

-- addVector with pattern matching (2-nd implementation)
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
--addVectors a b = (fst a + fst b, snd a + snd b)
-- better implementation with pattern matching
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- tuples third
first :: (a, b, c) -> a  
first (x, _, _) = x  
--  
second :: (a, b, c) -> b  
second (_, y, _) = y  
--  
third :: (a, b, c) -> c  
third (_, _, z) = z  

-- lisct comprehension and pattern matching
xs       = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
list11   =  [a+b | (a,b) <- xs]           -- [4,7,6,8,11,4]   

-- tell  
tell          :: (Show a) => [a] -> String  
tell []       = "The list is empty"  
tell (x:[])   = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_)  = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

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






{-
-- division
divide :: Int -> Int -> Int
--divide a b = 
--    if a >= b 
--      then
--        0
--      else
--        100
divide a b | a > b  = 0
           | a == b = 1
           | othewise 100

--x = 0 :: Int
--n = 0 :: Int 
--allMult a b = (a * (add1 n)): [x]
allMult a b | a * n < b  -- continue increasing n
            | a * n == b = n 
            | a * n > b -- stop cycle
-}

{-
--allMultiples :: [Int, Int] -> [Int]
--allMultiples [a, b] = where

--  
--      let m = [1 .. ] 
--      let n = add1 n  
--a = 9
--x = take 10 (iterate (a*)1) 
-}
