module Lib4
    ( someFuncLib4
    ) where

--import Prelude hiding (max, signum)

import Data.List
--import Data.List (nub, sort)   -- if we want only these functions
--import Data.List hiding (nub)  -- if we want everything, except of nub
--import qualified Data.Map      -- if we want call funcs like "Data.Map.nub"
--import qualified Data.Map as M -- if we want call funcs like "M.nub"  

import Data.Function

import Data.Char
import qualified GHC.Unicode as U 
import qualified Data.Map as Map 
import qualified Data.Set as Set

import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube   as Cube

import qualified Geometry as Geom

import Shapes

import Data.String
import Data.Int
--import GHC.Int
--import data-easy
import Data.Char (toUpper)
import Control.Monad 
--import Data.Map 

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
  putStrLn "\n"
  putStrLn ".......simpliest func with Lambda syntaxis .........." 
  putStrLn "id :: a -> a    -- type singnature"
  putStrLn "id x = x        -- regular definition"
  putStrLn "id = \\x -> x    -- lambda syntaxis definition"  -- prints id = \x -> x
  putStrLn "Anonimus function devinision, aka labmda-expression"  
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
  putStrLn "\n"
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
  putStrLn "\n"
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
  putStrLn "\n"
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
  putStrLn "\n"
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
  putStrLn "\n"
  putStrLn "--- All numbers from 50 to 100 whose remainder when divided with the number 7 is 3"
  putStr $ show $ [ x | x <- [50..100], x `mod` 7 == 3]  -- [52,59,66,73,80,87,94]
  putStrLn ", [ x | x <- [50..100], x `mod` 7 == 3]"   -- [52,59,66,73,80,87,94]
  putStrLn "\n"
  putStrLn " -- All numbers from 10 to 20 that are not 13, 15 or 19"
  putStr $ show $ [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]  
  putStrLn ", [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]"  -- [10,11,12,14,16,17,18,20]  
  putStrLn "\n"
  putStrLn "----- All the possible combinations between numbers in two lists ------"
  putStr $ show $ [ x*y | x <- [2,5,10], y <- [8,10,11]]  
  putStrLn ", [ x*y | x <- [2,5,10], y <- [8,10,11]]"   -- [16,20,22,40,50,55,80,100,110] 
  putStrLn "\n --This function replaces every element of a list with 1 and then sums that up"
  putStr $ show $ length' "a2b4c6"
  putStrLn ", length' xs = sum [1 | _ <- xs],  length' \"a2b4c6\""    -- 6
  putStrLn "\n"
  putStrLn "\n--Function that takes a string and removes everything except uppercase letters from it"
  putStr $ show $ removeNonUppercase "EveryThing is GooD!"
  putStrLn ", removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']],   \"EveryThing is GooD!\""
  putStrLn "\n"
  putStrLn "\n-- Remove all odd numbers without flattening the list."
  putStrLn "xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]"  
  putStr $ show $ [ [ x | x <- xs, even x ] | xs <- xxs] 
  putStrLn ", [ [ x | x <- xs, even x ] | xs <- xxs]"     -- [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]] 
  putStrLn "\n"
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
{-  
  putStrLn "\n------------- fromIntegral ---------------------" 
  putStr $ show $ fromIntegral (minBound :: Int16) + 3.2       -- -32764.8
  putStrLn ", fromIntegral (minBound :: Int16) + 3.2"
  putStrLn "But, (minBound :: Int16) + 3.2 -- compiler error"
-}  
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
  specShow (tell ([] :: [()]), tell [1], tell[1,2], tell[1,2,3])
           ", tell ([] :: [()]), tell [1], tell[1,2], tell[1,2,3]\n"
           "Pattern Matching tell function"
  specShow ((length ("a" :: String)), (length' "ab"), (length'' "abc"))
           ", length (\"a\" :: String), length' \"ab\", length'' \"abc\"\n"           
           "different length funcs"
  specShow ((all (<10) [1,3,5,7,9]), (all even [2,4,6,8,10]), (all ( \x -> (x*x)/4 > 10) [5,10,15]))
           ", (all (<10) [1,3,5,7,9]), (all even [2,4,6,8,10]), (all ( \\x -> (x*x)/4 > 10) [5,10,15])\n"           
           "Functinon all "
  specShow (calcBmis [(80, 1.8), (85, 1.9), (65, 1.64)])
           ", calcBmis [(80, 1.8), (85, 1.9), (65, 1.64)]\n"
           "calcBmis with guards"
  specShow ([let square x = x * x in (square 5, square 3, square 2)])
           ", [let square x = x * x in (square 5, square 3, square 2)]\n"          
           "let <Bindings> in <Expression>"
  specShow ((let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar))
           ", (let a = 100; b = 200; c = 300 in a*b*c, let foo=\"Hey \"; bar = \"there!\" in foo ++ bar)\n"
           "more let <Bindings> in <Expression>"
  specShow ((let (a,b,c) = (1,2,3) in a+b+c) * 100)
           ", (let (a,b,c) = (1,2,3) in a+b+c) * 100\n"
           "more let <Bindings> in <Expression>"
  specShow ((quicksort [10,2,4,4,8,9]), 
            (quicksort "the quick brown fox jumps over the lazy dog"), 
            (quicksort [(10,2,5),(3,1,6),(-99,4,2),(3,4.0,8),(-99,5,-100)]), 
            (quicksort [10,2,5,3,1,6,-99,4,2,3,4.0,8,9]))
           "\nquicksort [10,2,4,4,8,9]\nquicksort \"the quick brown fox jumps over the lazy dog\"\n\
           \quicksort [(10,2,5), (3,1,6), (-99,4,2), (3,4.0,8), (-99,5,-100)]\n\
           \quicksort [10,2,5,3,1,6,-99,4,2,3,4.0,8,9]"
           "Recursions"
  specShow ((multThree 3 5 9), ( ((multThree 3) 5) 9) )
           ",  (multThree 3 5 9), (((multThree 3) 5) 9)\n\
           \multThree :: (Num a) => a -> a -> a -> a == multThree :: (Num a) => a -> (a -> (a -> a))"
           "curry"
  specShow (multTwoWithNine 2 3)
           ", (multTwoWithNine 2 3), where (multTwoWithNine = multThree 9)\n\
           \and (multThree x y z) -- should be 3 parameters "
           "curry, using function with too few parameters\n\
           \---------------- or partialy applyed function"
  specShow ((compare 10 9), ((compare 10) 9))
           ", compare 10 9, (compare 10) 9\n"
           "curry, using function with too few parameters\n\
           \---------------- or partialy applyed function"
  specShow ((200 / 10), (divideByTen 200), ((/10) 200))
           ", \"200 / 10\" == \"divideByTen 200\" == \"(/10) 200\"\n"
           "partialy applyed infix function"
  specShow ((applyTwice (+3) 10), 
            (applyTwice (++ " HAHA") "hey"), (applyTwice ("HAHA " ++) "hey"),
            (applyTwice (multThree 3 4) 10), (applyTwice (3:) [1]))
           ",\n(applyTwice (+3) 10), (applyTwice (++ \" HAHA\") \"hey\"), (applyTwice (\"HAHA \" ++) \"hey\")\n\
           \(applyTwice (multThree 3 4) 10), (applyTwice (3:) [1])"
           "applyTwice again"
  specShow ((zipWith' (+) [1,2,3] [8,9,0]),
            (zipWith' (\x y -> 2*x + y) [1..4] [5..8]), 
            (zipWith' (/) [8,9,9] [1,2,3]),
            (zipWith' (**) (replicate 10 5) [1..10]),
            (zipWith' (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]))
           ", \nzipWith' (+) [1,2,3] [8,9,0] \nzipWith' (\\x y -> 2*x + y) [1..4] [5..8]\n\
           \nzipWith' (/) [8,9,9] [1,2,3] \nzipWith' (**) (replicate 10 5) [1..10] \n\
           \zipWith' (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]"
           "zipWith' using a lambda notation in a second example"
  specShow ((gcd 12 8), (gcd 12 (gcd 16 8)) )          
           ", (gcd 12 8) (gcd 12 (gcd 16 8))\n"
           "gcd greates common divisor"
  specShow ((flip (/) 1 2), (flip (>) 3 5), (flip mod 3 6))
           ", flip (/) 1 2), flip (>) 3 5), flip mod 3 6\n"           
           "flip"
  specShow ((gcd'' 12 36 96), 
            (gcd' 12 $ gcd' 36 96), 
            (gcd' (gcd' 12 36) 96), 
            (gcd' (gcd' 12 96) 36))
           ",\n(gcd'' 12 36 96)\n(gcd' 12 $ gcd' 36 96)\n(gcd' (gcd' 12 36) 96)\n\
           \(gcd' (gcd' 12 96) 36)\n"
           "gcd'' 13 36 96  -- gcd'' with 3 agruments"
  specShow ( (5 `elem` [1,5,6]), (elem 5 [1,5,6]), 
             ([1, 2, 3]), (1:2:3:[]) )
           ", \n(5 `elem` [1,5,6]), sugar for  (elem 5 [1,5,6])\n\
           \[1, 2, 3]), sugar for  (1:2:3:[])\n"            
           "Syntactic Sugar"           
  specShow ((lcm' 5 12), (lcm' (-6) 8))
           ", (lcm' 5 12), (lcm' (-6) 8)\n"           
           "lcm -- lowest common multiple"
  specShow ((lcm'' 12 4 5), (lcm' 12 $ lcm' 4 5), (lcm' 5 $ lcm' 12 4))
           ", (lcm'' 12 4 5), (lcm' 12 $ lcm' 4 5), (lcm' 5 $ lcm' 12 4)\n"
           "lcm'' with 3 arguments"
  specShow ((map abs [-1,-3,4,-12]), 
            (map reverse ["abc","cda","1234"]),
            (map (3*) [1,2,3,4] ),
            (map (recip . negate) [1,4,-5,0.1]),
            (map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]) )
           "\nmap abs [-1,-3,4,-12]\nmap reverse [\"abc\"\"cda\",\"1234\"]),\n\
            \map (3*) [1,2,3,4] )\nmap (recip . negate) [1,4,-5,0.1])\n\            
            \map (\\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)])\n"            
           "map"
  specShow ((foldl (/) 64 [4,2,4]), 
            (foldl (/) 3 []), 
            (foldl max 5 [1,2,3,4]), 
            (foldl max 5 [1,2,3,4,5,6,7]), 
            (foldl (\x y -> 2*x + y) 4 [1,2,3]))
           "\nfoldl (/) 64 [4,2,4]\nfoldl (/) 3 []\nfoldl max 5 [1,2,3,4]\n\ 
           \foldl max 5 [1,2,3,4,5,6,7]\n(foldl (\\x y -> 2*x + y) 4 [1,2,3]"
           "foldl"
  specShow "!!!!!    "
           "(foo a = bar b a)\" == \"(foo = bar b)\"     !!!!!\n\
           \\"sum1 xs = foldl (\\acc x -> acc + x) 0 + xs\"  == \"sum2 = foldl (+) 0\"\
           \, NO xs here! it can be omited"
           "Succincity, because of CURRYING !!!"
  specShow ((filter2 (>5) [1,2,3,4,5,6,7,8] ),
            (filter2 odd [3,6,7,9,12,14]),
            (filter2 (\x -> length (x :: String) > 4) ["aaaa","bbbbbbbbbbbbb","cc"]),
            (filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"),
            (filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"))
           "\nfilter' (>5) [1,2,3,4,5,6,7,8]\nfilter' odd [3,6,7,9,12,14]\n\
           \filter2 (\\x -> length (x :: String) > 4) [\"aaaa\",\"bbbbbbbbbbbbb\",\"cc\"]\n\
           \filter (`elem` ['a'..'z']) \"u LaUgH aT mE BeCaUsE I aM diFfeRent\"\n\
           \filter (`elem` ['A'..'Z']) \"i lauGh At You BecAuse u r aLL the Same\""
           "filter"
  specShow ((sum (takeWhile (<10000) (filter odd (map (^2) [1..])))),
           (sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])))
           ", sum (takeWhile (<10000) (filter odd (map (^2) [1..])))\n\
           \sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)]) - list comprehention"
           "The sum of all odd squares that are smaller than 10,000.\n\
           \First, we'll begin by mapping the (^2) function to the infinite list [1..].\n\
           \Then we filter them to get the odd ones. Then, we'll take elements from that list\n\
           \while they are smaller than 10,000. Finally, we'll get the sum of that list."
  specShow ((map (+ 0) [1, 3, 3, 4]), (map (* 0) [1, 3, 3, 4]), 
            (map (/ 1) [1, 3, 3, 4]), (map (/ 0) [1, 3, 3, 4]))
           ", \nmap (+ 0) [1, 3, 3, 4]\nmap (* 0) [1, 3, 3, 4]\n\
           \map (/1 0) [1, 3, 3, 4]\nmap (/ 0) [1, 3, 3, 4]"
           "simple map ..."
  specShow ( ((map (*) [0..]) !! 4) 5)
           ", (map (*) [0..]) !! 4) 5"           
           "some trics using map"
  specShow ((length (filter (\xs -> length xs > 15) (map chain [1..100]))))
           ", length (filter (\\xs -> length xs > 15) (map chain [1..100]))"
           "Collatz sequences"
  specShow ((fnAddTwo 8 6), (fnAddLong 12), (fnAddShort 12) )
           ", (fnAddTwo 8 6)   fnAddLong 12)   (fnAddShort 12)\n\
           \defnitions ... fnAddLong n = fnAddTwo 10 n ... fnAddShort = fnAddTwo 10\n\
           \(fnAddLong n = fnAddTwo 10 n) == (fnAddShort = fnAddTwo 10) !!!"
           "(foo a = bar b a) == (foo = bar b)"
  specShow ((elem2 9 [1,2,3,4,5,6,7]), (elem3 5 [1,2,3,4,5,6,7]))
           ", (elem2 9 [1,2,3,4,5,6,7]), (elem3 5 [1,2,3,4,5,6,7])\n\
           \elem2 y ys = foldl (\\acc x -> if x == y then True else acc) False ys\n\
           \elem3 y ys = foldr (\\x acc -> if x == y then True else acc) False ys"
           "elem implementaion with foldl and foldr"
  specShow ((foldl (/) 64 [4,2,4]), (foldr (/) 64 [4,2,4]), 
           (foldl (+) 5 [1,2,3,4]), (foldr (+) 5 [1,2,3,4]),
           (foldl (\x y -> (x+y)/2) 54 [12,4,10,6]), (foldr (\x y -> (x+y)/2) 54 [12,4,10,6]) )
           ", \n(foldl (/) 64 [4,2,4]) vs (foldr (/) 64 [4,2,4])\n\
           \(foldl (+) 5 [1,2,3,4]), (foldr (+) 5 [1,2,3,4])\n\
           \(foldl (\\x y -> (x+y)/2) 54 [12,4,10,6]), (foldr (\\x y -> (x+y)/2) 54 [12,4,10,6])"
           "foldl vs foldr"
  specShow ((take 10 (enumFrom 'a') ), (take 10 (enumFrom 23)), 
           (enumFrom BB), (enumFrom Green') )
           ",\ntake 10 (enumFrom 'a')\ntake 10 (enumFrom 23)\n\
           \     data XXX = AA|BB|CC|DD deriving (Enum, Show)\n\
           \enumFrom BB\n\
           \     data Color'  = Blue' | Green' | Read' deriving (Show, Read, Eq, Enum)\n\
           \enumFrom Green'"
           "EnumFrom"
  specShow ((sum (filter (> 10) (map (*2) [2..10]))), 
           (sum $ filter (> 10) $ map (*2) [2..10]), 
           (map ($ 3) [(4+), (10*), (^2), sqrt]))
           ",\n#1. sum (filter (> 10) (map (*2) [2..10])) ==  sum $ filter (> 10) $ map (*2) [2..10]\n\
           \#2. map ($ 3) [(4+), (10*), (^2), sqrt] !!! mapping over list of\  \ functions !!!"
           "function application $"
  specShow (((negate . abs) (-1)), ((reverse . take 10 . enumFrom) 10), 
           ((abs . snd)(-1,-3)), (((2+).(3*).(4-)) 2) )
           ", \n(negate . abs) (-1)\n(reverse . take 10 . enumFrom) 10\
           \\n(abs . snd)(-1,-3)\n((2+).(3*).(4-)) 2"
           "function composition (.)"
  specShow ((map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]),
            (map (negate . abs) [5,-3,-6,7,-3,2,-19,24]))
           ", \nmap (\\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24], using lambda\n\
           \map (negate . abs) [5,-3,-6,7,-3,2,-19,24], using function composition"
           "function composition or lambda?"
  specShow ((sum (replicate 5 (max 6.7 8.9))), ((sum.replicate 5 . max 6.7) 8.9), 
            (sum . replicate 5 . max 6.7 $ 8.9) )
           ", \nsum (replicate 5 (max 6.7 8.9))\n\
           \sum.replicate 5 . max 6.7) 8.9\n\
           \sum . replicate 5 . max 6.7 $ 8.9"
           "point free style of function composition"
  specShow ((ceiling 3.000001), (ceiling 3), (ceiling (-3.7)), 
           (cos pi/3), (tan pi/16))
           "\nceiling 3.000001, ceiling 3, ceiling (-3.7)\n\
           \cos pi/3, tan pi/16"
           "ceiling --- cos --- tan"

  putStrLn "\n==================== import Data.List ===================="

  specShow ((nub [1,2,3,4,3,2,1,2,4,3,5]),(nub "asdfsadsa"),(nub [1,2.0,3,4,3,2,1,2,4,3,5]))
           "\nnub [1,2,3,4,3,2,1,2,4,3,5]\nnub \"asdfsadsa\"\nnub [1,2.0,3,4,3,2,1,2,4,3,5]"
           "nub"
  specShow ((intersperse '.' "MONKEY"), (intersperse 0 [1,2,3,4,5,6]),
            (intercalate " " ["hey","there","guys"]), 
            (intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]))
           "\nintersperse '.' \"MONKEY\"\nintersperse 0 [1,2,3,4,5,6]\n\
           \intercalate \" \" [\"hey\",\"there\",\"guys\"]\n\
           \intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]"
           "intersperse  --- intercalate"
  specShow ((transpose [[1,2,3],[4,5,6],[7,8,9]] ), (transpose ["hey","there","guys"]),
            (map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]))
           "\ntranspose [[1,2,3],[4,5,6],[7,8,9]]\ntranspose [\"hey\",\"there\",\"guys\"]\n\
           \map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]"
           "transposes  -- looks like matrix operation"      
  specShow ((concat ["foo","bar","car"]), (concat [[3,4,5],[2,3,4],[2,1,1]]),
            (concatMap (replicate 4) [1..3]), (concat ( map (replicate 4) [1..3])))
           "\nconcat [\"foo\",\"bar\",\"car\"]\nconcat [[3,4,5],[2,3,4],[2,1,1]]\n\
           \concatMap (replicate 4) [1..3] == concat ( map (replicate 4) [1..3])"
           "concat --- concatMap"           
  specShow ((and $ map (>4) [5,6,7,8]), (and $ map (==4) [4,4,4,3,4]), 
            (or $ map (==4) [2,3,4,5,6,1]), (or $ map (>4) [1,2,3]))
           "\nand $ map (>4) [5,6,7,8]\nand $ map (==4) [4,4,4,3,4]\
           \\nor $ map (==4) [2,3,4,5,6,1]\nor $ map (>4) [1,2,3]"
           "and --- or"
  specShow ((any (==4) [2,3,5,6,1,4]),(any (`elem` ['A'..'Z']) ("HEYGUYSwhatsup" :: [Char])),
            (all (`elem` ['A'..'Z']) ("HEYGUYSwhatsup" :: [Char])),(all (>4) [6,9,10] ))
           "\nany (==4) [2,3,5,6,1,4]\nany (`elem` ['A'..'Z']) (\"HEYGUYSwhatsup\" :: [Char])\
           \\nall (`elem` ['A'..'Z']) (\"HEYGUYSwhatsup\" :: [Char])\nall (>4) [6,9,10]\n"
           "any --- all"
  specShow (('o' `elem` ("Zvon" :: [Char])),('o' `elem` ['Z','v','o','n']),
            (elem 'o' ("aSdlkfjo"::[Char])))
           "\n'o' `elem` (\"Zvon\" :: [Char])\n'o' `elem` ['Z','v','o','n']\n\
           \elem 'o' (\"aSdlkfjo\"::[Char])"
           "intermission `elem` elem"
  specShow ((take 10 $ iterate (*2) 1),(take 3 $ iterate (++ "haha") "haha"))
           "\ntake 10 $ iterate (*2) 1\ntake 3 $ iterate (++ \"haha\") \"haha\""
           "iterate"
  specShow ((splitAt 3 "heyman"),(splitAt 100 "heyman"),(splitAt (-3) "heyman"),
            (let (a,b) = splitAt 3 "foobar" in b ++ a),
            --(do b ++ a where (a,b) = splitAt 3 "foobar") -- does not compile
            (rsDtL28') )
           "\nsplitAt 3 \"heyman\"\nsplitAt 100 \"heyman\"\nsplitAt (-3) \"heyman\"\n\
           \let (a,b) = splitAt 3 \"foobar\" in b ++ a\n\
           \do b ++ a where (a,b) = splitAt 3 \"foobar\""
           "splitAt"
  specShow ((takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]), 
            (takeWhile (/=' ') "This is a sentence"), 
            (sum $ takeWhile (<10000) $ map (^3) [1..]))
           "\ntakeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]\n\
           \takeWhile (/=' ') \"This is a sentence\")\n\
           \sum $ takeWhile (<10000) $ map (^3) [1..]"
           "takeWhile"
  specShow ((dropWhile (/=' ') "This is a sentence"), (dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]))
           "\ndropWhile (/=' ') \"This is a sentence\"\n\
           \dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]"
           "dropWhile"
  specShow (span (/=' ') "This is a sentence",
            let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest)
           "\nspan (/=' ') \"This is a sentence\"\
           \\nlet (fw, rest) = span (/=' ') \"This is a sentence\" in \"First word:\" \
           \++ fw ++ \", the rest:\" ++ rest"
           "span"
  specShow ((break (==4) [1,2,3,4,5,6,7]), (span (/=4) [1,2,3,4,5,6,7]), 
            (sort [8,5,3,2,1,6,4,2]), (sort "This will be sorted soon"))
           "\nbreak (==4) [1,2,3,4,5,6,7]\nspan (/=4) [1,2,3,4,5,6,7]\n\
           \sort [8,5,3,2,1,6,4,2]\nsort \"This will be sorted soon\""
           "break span sort"
  specShow ((group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] ), 
            (map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]))
           "\ngroup [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]\n\
           \map (\\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]"
           "group"
  specShow ((inits "w00t" ), (tails "w00t" ), (let w = "w00t" in zip (inits w) (tails w)))
           "\ninits \"w00t\"\ntails \"w00t\"\n\
           \let w = \"w00t\" in zip (inits w) (tails w)"
           "inits tails"
  specShow ((search "cat" "im a cat burglar"), ("cat" `isInfixOf` "im a cat burglar"), 
            ("Cat" `isInfixOf` "im a cat burglar"), 
            (isInfixOf "cats" "im a cat burglar") )
           "\nsearch \"cat\" \"im a cat burglar\"\n\"cat\" `isInfixOf` \"im a cat burglar\"\
           \\n\"Cat\" `isInfixOf` \"im a cat burglar\"\nisInfixOf \"cats\" \"im a cat burglar\""
           "isInfixOf --- and example: our implementaion seachig a list for subset"
  specShow (("hey" `isPrefixOf` "hey there!" ), ("hey" `isPrefixOf` "oh hey there!"), 
            ("there!" `isSuffixOf` "oh hey there!"), 
            ("there!" `isSuffixOf` "oh hey there"))
           "\n\"hey\" `isPrefixOf` \"hey there!\"\n\"hey\" `isPrefixOf` \"oh hey there!\"\
           \\n\"there!\" `isSuffixOf` \"oh hey there!\"\n\"there!\" `isSuffixOf` \"oh hey there\""
           "isPrefixOf --- isSuffixOf"
  specShow ((partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"), 
            (partition (>3) [1,3,5,6,3,2,1,0,3,7]),
            (partition (`notElem` ['A'..'Z']) "BOBsidneyMORGANeddy"))
           "\npartition (`elem` ['A'..'Z']) \"BOBsidneyMORGANeddy\"\n\
           \partition (>3) [1,3,5,6,3,2,1,0,3,7]\n\
           \partition (`notElem` ['A'..'Z']) \"BOBsidneyMORGANeddy\""
           "partition --- elem --- notElem"
  specShow ((partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"), 
            (span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"), 
            (break (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"))
           "\npartition (`elem` ['A'..'Z']) \"BOBsidneyMORGANeddy\"\
           \\nspan (`elem` ['A'..'Z']) \"BOBsidneyMORGANeddy\"\n\
           \break (`elem` ['A'..'Z']) \"BOBsidneyMORGANeddy\""
           "important diff between partition and span and break"
  specShow ((find (>4) [1,2,3,4,5,6]), (find (>9) [1,2,3,4,5,6]))
           "\nfind (>4) [1,2,3,4,5,6]\nfind (>9) [1,2,3,4,5,6]"
           "find   (find :: (a -> Bool) -> [a] -> Maybe a)"
  specShow ((head (dropWhile (\(val,y,m,d) -> val < 1000) stock1)), 
            (find (\(val,y,m,d) -> val > 1000) stock1))
           "\nstock1  = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]\
           \\nhead (dropWhile (\\(val,y,m,d) -> val < 1000) stock1) -- unsafe\n\
           \find (\\(val,y,m,d) -> val > 1000) stock1  -- safe"
           "unsafe and safe implementations of stock1 function"
  specShow ((elemIndex 4 [1,2,3,4,5,6]), (10 `elemIndex` [1,2,3,4,5,6]), 
            ( ' ' `elemIndices` "Where are the spaces?"))
           "\nelemIndex 4 [1,2,3,4,5,6]\n10 `elemIndex` [1,2,3,4,5,6]\
           \\n ' ' `elemIndices` \"Where are the spaces?\""
           "elemIndex --- elemIndices"
  specShow ((findIndex (==4) [5,3,2,1,6,4]), (findIndex (==7) [5,3,2,1,6,4] ), 
            (findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"))
           "\nfindIndex (==4) [5,3,2,1,6,4]\nfindIndex (==7) [5,3,2,1,6,4] \
           \\nfindIndices (`elem` ['A'..'Z']) \"Where Are The Caps?\""
           "findIndex --- findIndices"
  specShow ((zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]),
            (zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]))
           "\nzip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]\n\
           \zipWith3 (\\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]"
           "zipWith3,4,5,6,7 --- zip3,4,5,6,7"
  specShow ((lines "first line\nsecond line\nthird line" ), 
            (unlines ["first line", "second line", "third line"]), 
            (words "hey these are the words in this sentence"), 
            (words "hey these           are    the words in this\nsentence"),
            (unwords ["hey","there","mate"] ))
           "\nlines \"first line\nsecond line\nthird line\"\n\
           \unlines [\"first line\", \"second line\", \"third line\"]\n\
           \words \"hey these are the words in this sentence\"\n\
           \words \"hey these           are    the words in this\nsentence\"\n\
           \unwords [\"hey\",\"there\",\"mate\"]"
           "lines --- unlines --- words --- unwords"
  specShow ((delete 'h' "hey there ghang!"), (delete 'h' . delete 'h' $ "hey there ghang!"), 
            (delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"), ())
           "\ndelete 'h' \"hey there ghang!\"\ndelete 'h' . delete 'h' $ \"hey there ghang!\"\n\
           \delete 'h' . delete 'h' . delete 'h' $ \"hey there ghang!\"\n"
           "delete"
  specShow (([1..10] \\ [2,5,9]), ("Im a big baby" \\ "big"), 
            (delete 2 . delete 5 . delete 9 $ [1..10]))
           "\n[1..10] \\\\ [2,5,9]\n\"Im a big baby\" \\\\ \"big\"\
           \\ndelete 2 . delete 5 . delete 9 $ [1..10] == [1..10] \\\\ [2,5,9]"
           "\\\\"
  specShow (("hey man" `union` "man what's up"), ([1..7] `union` [5..10] ), 
            ([1..7] `intersect` [5..10]))
           "\n\"hey man\" `union` \"man what's up\"\n[1..7] `union` [5..10]\
           \\n[1..7] `intersect` [5..10]"
           "union --- intersect"
  specShow ((insert 4 [3,5,1,2,8,2]), (insert 4 [1,3,4,4,1]), 
            (insert 4 [1,2,3,5,6,7]), (insert 'g' $ ['a'..'f'] ++ ['h'..'z']),
            (insert 3 [1,2,4,3,2,1]))
           "\ninsert 4 [3,5,1,2,8,2]\ninsert 4 [1,3,4,4,1]\n\
           \insert 'g' $ ['a'..'f'] ++ ['h'..'z']\ninsert 3 [1,2,4,3,2,1]"
           "insert"
  specShow ((let xs = [1..6] in sum xs / genericLength xs), 
            (groupBy (\x y -> (x > 0) == (y > 0)) rsDtL89),
            (groupBy ((==) `on` (> 0)) rsDtL89))
           "\nlet xs = [1..6] in sum xs / genericLength xs\n\
           \groupBy (\\x y -> (x > 0) == (y > 0)) value\n\
           \groupBy ((==) `on` (> 0)) value"
           "genericLength genericTake genericDrop genericSplitAt genericIndex genericReplicate\n\
           \nubBy deleteBy unionBy intersectBy groupBy AND 'on'"
  specShow ((sortBy (compare `on` length) [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]))
           "\nsortBy (compare `on` length) [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]"
           "sortBy AND 'on'"

  putStrLn "\n==================== import Data.Char ===================="
  putStrLn " All these predicates have a type signature of \"Char -> Bool\"\n"
  putStrLn "isControl checks whether a character is a control character."
  putStrLn "isSpace checks whether a character is a white-space characters. That includes spaces, tab characters, newlines, etc."
  putStrLn "isLower checks whether a character is lower-cased."
  putStrLn "isUpper checks whether a character is upper-cased."
  putStrLn "isAlpha checks whether a character is a letter."
  putStrLn "isAlphaNum checks whether a character is a letter or a number."
  putStrLn "isPrint checks whether a character is printable. Control characters, for instance, are not printable."
  putStrLn "isDigit checks whether a character is a digit."
  putStrLn "isOctDigit checks whether a character is an octal digit."
  putStrLn "isHexDigit checks whether a character is a hex digit."
  putStrLn "isLetter checks whether a character is a letter."
  putStrLn "isMark checks for Unicode mark characters. Those are characters that combine with preceding letters"
  putStrLn "      to form latters with accents. Use this if you are French."
  putStrLn "isNumber checks whether a character is numeric."
  putStrLn "isPunctuation checks whether a character is punctuation."
  putStrLn "isSymbol checks whether a character is a fancy mathematical or currency symbol."
  putStrLn "isSeparator checks for Unicode spaces and separators."
  putStrLn "isAscii checks whether a character falls into the first 128 characters of the Unicode character set."
  putStrLn "isLatin1 checks whether a character falls into the first 256 characters of Unicode."
  putStrLn "isAsciiUpper checks whether a character is ASCII and upper-case."
  putStrLn "isAsciiLower checks whether a character is ASCII and lower-case."
  specShow ((all isAlphaNum ("bobby283" :: [Char])), 
            (all isAlphaNum ("eddy the fish!" :: String)), 
            (words "hey guys its me"), (groupBy ((==) `on` isSpace) "hey guys its me"),
            filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me")
           "\nall isAlphaNum (\"bobby283\" :: [Char])\nall isAlphaNum (\"eddy the fish!\" :: String)\
           \\nwords \"hey guys its me\"\ngroupBy ((==) `on` isSpace) \"hey guys its me\"\
           \\nfilter (not . any isSpace) . groupBy ((==) `on` isSpace) $ \"hey guys its me\""
           "isAlphaNum --- isSpace"
  specShow ((generalCategory someChar1 == Space),
            (generalCategory 'A'), (generalCategory '.'),
            (generalCategory '9'), (generalCategory 'b'))  
           "\nsomeChar1 = ' '\n\
           \generalCategory someChar1 == Space\ngeneralCategory 'A'\n\
           \generalCategory '.'\ngeneralCategory '9'\n\
           \generalCategory 'b'"
           "generalCategory"
  specShow ((map digitToInt "34538cF"), (map digitToInt "FF85AB"), 
            (intToDigit 15), (intToDigit 5 ))
           "\nmap digitToInt \"34538\"\nmap digitToInt \"FF85AB\"\
           \\nintToDigit 15\nintToDigit 5"
           "toUpper --- toLower --- toTitle --- digitToInt --- intToDigit"
  specShow ((toUpper 'a'), (toUpper '5'),(toUpper 'λ'), (U.toUpper 'б'),
            (toLower '𝔹'), (toUpper '𝔹'))
           "\ntoUpper 'a'\ntoUpper '5'\
           \\ntoUpper 'λ'\nU.toUpper 'б'\ntoLower '𝔹'\ntoUpper '𝔹'"
           "toUpper --- U.toUpper -- toLower"
  specShow ((ord 'a'), (ord 'λ'), (ord 'б'), (ord '哈'), (ord '𝔹'), 
            (chr 97), (chr 953), (chr 1041), (chr 90), (chr 120120))
           "\nord 'a', ord 'λ', ord 'б', ord '哈' ord '𝔹'\n\
           \chr 97, chr 953, chr 1041, chr 90, chr 120120"
           "ord --- chr"
  specShow ((encode' 5 "Marry Christmas! Ho ho ho!"), 
            (decode 3 "Lp#d#olwwoh#whdsrw"))
           "\nencode 5 \"Marry Christmas! Ho ho ho!\"\n\
           \decode 3 \"Lp#d#olwwoh#whdsrw\"\n\
           \\"encode' shift msg =  map (chr . (+ shift) . ord) msg\"\n\
           \\"decode shift msg = encode (negate shift) msg\""
           "encode' --- decode --- example"

  putStrLn "\n==================== import Data.Map ===================="
  specShow ((findKey "penny" phoneBook), (findKey "wilma" phoneBook), 
            (Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]), 
            (Map.fromList [(1,2),(3,4),(3,2),(5,5)]))
           "\nfindKey \"penny\" phoneBook\nfindKey \"wilma\" phoneBook\
           \\nMap.fromList [(\"betty\",\"555-2938\"),(\"bonnie\",\"452-2928\"),(\"lucille\",\"205-2928\")]\
           \Map.fromList [(1,2),(3,4),(3,2),(5,5)]\
           \findKey' key = foldr (\\(k,v) acc -> if key == k then Just v else acc) Nothing"
           "custom 'findKey' --- Map.fromList"
  -- N.B. Could not put Map.empty into specShow !!! failed to compile
  specShow (({-Map.empty does not compile-} ), (Map.insert 3 100 Map.empty), 
            (Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))), 
            (Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty), 
            (Map.null Map.empty), (Map.null $ Map.fromList [(2,3),(5,5)]))
           "\nMap.empty\nMap.insert 3 100 Map.empty\n\
           \Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))\
           \nMap.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty\
           \\nMap.null Map.empty  -- checks if map is empty?\
           \nMap.null $ Map.fromList [(2,3),(5,5)] - check if map is empty?"
           "Map.empty --- Map.insert --- Map.null"           
  specShow ((Map.size Map.empty), (Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]), 
            (Map.singleton 3 9), (Map.insert 5 9 $ Map.singleton 3 9), 
            (Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]), (Map.member 3 $ Map.fromList [(2,5),(4,5)]) )
           "\nMap.size Map.empty\nMap.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]\nMap.singleton 3 9\
           \\nMap.insert 5 9 $ Map.singleton 3 9\nMap.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]\n\
           \Map.member 3 $ Map.fromList [(2,5),(4,5)"
           "Map.size --- Map.singleton --- Map.member"
  specShow ((Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]), 
            (Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]), 
            (Map.toList . Map.insert 9 2 $ Map.singleton 4 3))
           "\nMap.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]\nMap.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]\n\
           \Map.toList . Map.insert 9 2 $ Map.singleton 4 3\n\n\n"
           "Map.map --- Map.filter --- Map.toList"
  specShow ((Map.lookup "patsy" $ phoneBookToMap phoneBook'), (Map.lookup "wendy" $ phoneBookToMap phoneBook'), 
            (Map.lookup "patsy" $ phoneBookToMap' phoneBook'))
           "\nMap.lookup \"patsy\" $ phoneBookToMap phoneBook'\nMap.lookup \"wendy\" $ phoneBookToMap phoneBook'\n\
           \Map.lookup \"patsy\" $ phoneBookToMap' phoneBook'\n\n\n"
           "Map.lookup"
  specShow ((Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)] ), 
            (Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]), 
            (Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]))
           "\nMap.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]\
           \\nMap.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]\
           \nMap.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]"
           "Map.fromListWith --- Map.insertWith"

  putStrLn "\n==================== import Data.Set ======================"
  putStrLn "text1 = \"I just had an anime dream. Anime... Reality... Are they so different?\""
  putStrLn "text2 = \"The old man left his garbage can out and now his trash is all over my lawn!\""
  specShow ((Set.fromList text1), (Set.fromList text2), (Set.intersection rsDtSt1 rsDtSt2),
            (Set.difference rsDtSt1 rsDtSt2), (Set.difference rsDtSt2 rsDtSt1),
            (Set.union rsDtSt1 rsDtSt2))
           "\n(Set.fromList text1\n(Set.fromList text2\nSet.intersection rsDtSt1 rsDtSt2\
           \Set.difference rsDtSt1 rsDtSt2\nSet.difference rsDtSt2 rsDtSt1\n\
           \Set.union rsDtSt1 rsDtSt2\n\
           \text1=\"I just had an anime dream. Anime... Reality... Are they so different?\"\n\
           \text2=\"The old man left his garbage can out and now his trash is all over my lawn!\""
           "fromList --- intersection --- difference --- union"
  specShow ((Set.null Set.empty), (Set.null $ Set.fromList [3,4,5,5,4,3]), (Set.singleton 9), 
            (Set.size $ Set.fromList [3,4,5,3,4,5]), (Set.insert 4 $ Set.fromList [9,3,8,1]),
            (Set.insert 8 $ Set.fromList [5..10]), (Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]))
           "\nSet.null Set.empty\nSet.null $ Set.fromList [3,4,5,5,4,3]\nSet.singleton 9\n\
           \Set.size $ Set.fromList [3,4,5,3,4,5]\nSet.insert 4 $ Set.fromList [9,3,8,1]\n\
           \Set.insert 8 $ Set.fromList [5..10]\nSet.delete 4 $ Set.fromList [3,4,5,4,3,4,5]"
           "null --- size --- member --- empty --- singleton --- insert --- delete"
  specShow ((Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]), 
            (Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]), 
            (Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]), 
            (Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]))
           "\nSet.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]\n\
           \Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]\n\
           \Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]\n\
           \Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]"
           "isSubsetOf --- isProperSubsetOf"
  specShow ((setNub "HEY WHATS CRACKALACKIN"), (nub "HEY WHATS CRACKALACKIN"))
           "\nsetNub \"HEY WHATS CRACKALACKIN\"\nnub \"HEY WHATS CRACKALACKIN\"\
           \n  setNub xs = Set.toList $ Set.fromList xs"
           "toList --- custom 'setNub'"

  putStrLn "\n==================== Making your own modules =============="
  putStrLn "see file Geometry.hs -- Version #1"
  putStrLn "see Directory Geometry and files: Cube.hs, Sphere.hs, Cuboid.hs -- Version #2"
  specShow ((Geom.sphereVolume 10), (Geom.cubeArea 10), (Geom.cuboidVolume 5.1 6.2 7.3),
            (Sphere.volume 10), (Cube.area 10), (Cuboid.volume 5.1 6.2 7.3))
           "\nGeom.sphereVolume 10\nGeom.cubeArea 10\nGeom.cuboidVolume 5.1 6.2 7.3\
           \\nSphere.volume 10\nCube.area 10\nCuboid.volume 5.1 6.2 7.3"
           "modules Geom and Sphere, Cube, Cuboid"

  putStrLn "\n=========== Making your own types and Typeclasses ==========="
  specShow ((neo1), (trin1), (morf1), (neo2),
            (morf1 == neo1), (neo1 > trin1), (neo2 > neo1))
           "\nmorf1 == neo1\nneo1 > trin1\n\
           \neo2 > neo1"
           "data Person"
  specShow ((Car "lexus"  "RX350"  2014), (Car {company = "ford", model = "Mustang", year = 1967}), 
            (rsMyDt18 < c1 ), (tellCar c1), (tellCar rsMyDt18))
           "\nc1 = Car \"lexus\"  \"RX350\"  2014\n\
           \rsMyDt18 = Car {company = \"ford\", model = \"Mustang\", year = 1967}\n\
           \rsMyDt18 < c1\ntellCar c1\ntellCar rsMyDt18"
           "data Car"
  specShow ((Vector 3 5 8 `vplus` Vector 9 2 8), 
            (Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3), 
            (Vector 3 9 7 `vectMult` 10), 
            (Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0), 
            (Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)))
           "\nVector 3 5 8 `vplus` Vector 9 2 8\n\
           \Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3\n\
           \Vector 3 9 7 `vectMult` 10\nVector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0\n\
           \Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)"
           "data Vector (3D Vector)"
  specShow (((read "Person {firstName =\"Neo\", lastName =\"Anderson\", age = 40, height = 184, phoneNumber = \"555-555\", flavor = \"butter-scotch\" }" :: Person) == neo1), 
            ((read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43, height = 178, phoneNumber = \"555-999\", flavor = \"vanilla\" }" :: Person) > neo1) )
           "(read \"Person {firstName =\"Neo\", lastName =\"Anderson\", age = 40, height = 184, phoneNumber = \"555-555\", flavor = \"butter-scotch\" }\" :: Person) == neo1\n\
           \(read \"Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43, height = 178, phoneNumber = \"555-999\", flavor = \"vanilla\" }\" :: Person) > neo1"           
           "read 'custom' --- Derived instances"
  specShow ((read "Just 80" :: Maybe Int), (read  "Just 't'" :: Maybe Char), 
            (read "Nothing" :: Maybe Int), (read  "Nothing" :: Maybe Char) )
           "\nread \"Just 80\" :: Maybe Int\nread  \"Just 't'\" :: Maybe Char\
           \\nread \"Nothing\" :: Maybe Int\nread  \"Nothing\" :: Maybe Char"
           "read Maybe --- Derived instances"
  specShow ((True `compare` False), (True > False), 
            (Nothing < Just 100), (Nothing > Just (-49999)), 
            (Just 3 `compare` Just 2), (Just 100 > Just 50))
           "\nTrue `compare` False\nTrue > False\nNothing < Just 100\
           \\nNothing > Just (-49999)\nJust 3 `compare` Just 2\n\
           \Just 100 > Just 50"
           "compare Bool --- Maybe"
  specShow ((show Wednesday), (read "Saturday" :: Day), (Saturday == Sunday), 
            (Monday `compare` Wednesday), (minBound :: Day), (succ Monday),
            (pred Saturday), ([Thursday .. Sunday]) )
           "\nshow Wednesday\nread \"Saturday\" :: Day\nSaturday == Sunday\
           \\nMonday `compare` Wednesday\nminBound :: Day\nsucc Monday\n\
           \[Thursday .. Sunday]\n[minBound .. maxBound] :: [Day]\n\
           \N.B. succ Sunday *** Exception: succ{Day}: tried to take `succ' of last tag in enumeration\n\
           \N.B. pred Monday *** Exception: pred{Day}: tried to take `pred' of first tag in enumeration"
           "Data Day --- deriving (Eq, Ord, Show, Read, Bounded, Enum)"  
  specShow ((Right 20 :: Either () Int) 
            ,(Right 'a' :: Either () Char)
            ,(Right 3.423 :: Either () Double)
--           ,((Left "w00t" :: Either () String))     -- does not compile
--           ,(Left True :: Either () Bool)           -- does not compile
--           ,(Left "Something" :: Either () String)) -- does not compile
            )
           "\nRight 20\nRight 'a' :: Either a Char\
           \\nRight 3.423 :: Either () Double"
           "data Either a b = Left a | Right b"
  specShow ((eitherHead [1,2,3,4,5]), (eitherHead "This is a test string") )
           "\neitherHead [1,2,3,4,5]\neitherHead \"This is a test string\"\n\
            \eitherHead :: [a] -> Either String a\n\
            \eitherHead [] = Left \"Empty Head\"\n\
            \eitherHead (x:xs) = Right x"
           "safe head implementation, using Either"
  specShow ((lockerLookup 101 lockers)
           ,(lockerLookup 100 lockers)
           ,(lockerLookup 102 lockers)
           ,(lockerLookup 110 lockers)
           ,(lockerLookup 105 lockers))
           "\nlockerLookup 101 lockers\nlockerLookup 100 lockers\nlockerLookup 102 locker\
           \\nlockerLookup 110 lockers\nlockerLookup 105 lockers"
           "lockers example, using Either"
  specShow ((3:(4:(5:6:[]))), (3:4:5:6:[]), ([3,4,5,6]))
           "\n(3:(4:(5:6:[]))\n3:4:5:6:[]\n[3,4,5,6]"
           "recursive data structure"
  specShow ((foldr treeInsert EmptyTree [3,99, (-1), 10, 11, 6, 8, 77, 100, (-5)])
           ,(8 `treeElem` numsTree1)
           ,(1100 `treeElem` numsTree1)
           ,(1 `treeElem` numsTree1)
           ,((-1) `treeElem` numsTree))
           "\nnumsTree1 = foldr treeInsert EmptyTree [3,99, (-1), 10, 11, 6, 8, 77, 100, (-5)]\
           \n8 `treeElem` numsTree1\n1100 `treeElem` numsTree1\n\
           \1 `treeElem` nnumsTree1)\n(-1) `treeElem` numsTree"
           "binary tree search - data Tree"
  specShow ((yesno $ length []), (yesno ("haha" :: [Char])) ,(yesno ("" :: [Char]))
           ,(yesno $ Just 0), (yesno True), (yesno EmptyTree), (yesno []), (yesno [0,0,0]),
           (yesno (Green :: TrafficLight)))
           "\nyesno $ length []\nyesno (\"haha\" :: [Char])\nyesno (\"\" :: [Char])\
           \\nyesno $ Just 0\nyesno True\nyesno EmptyTree\nyesno []\nyesno [0,0,0]\
           \\nyesno (Green :: TrafficLight)"
           "Class \"YesNo\" Typeclass Example,  \"yesno :: (YesNo a) => a -> Bool\""
  specShow ((yesnoIf [] "YEAH!" "NO!"), (yesnoIf [2,3,4] "YEAH!" "NO!") ,(yesnoIf True "YEAH!" "NO!"),
           (yesnoIf (Just 500) "YEAH!" "NO!"), (yesnoIf Nothing "YEAH!" "NO!"))
           "\nyesnoIf [] \"YEAH!\" \"NO!\"\nyesnoIf [2,3,4] \"YEAH!\" \"NO!\"\n\
           \yesnoIf True \"YEAH!\"\"NO!\"\nyesnoIf (Just 500) \"YEAH!\" \"NO!\"\n\
           \yesnoIf Nothing \"YEAH!\" \"NO!\""
           "function yesnoIf, Typeclass Example, it mimics if statement"
  
  putStrLn "\n======================== IO () ========================\n"
  
  putStrLn "\n-------------- sequence (map print [1,2,3,4,5]) -----------"
  sequence (map print [1,2,3,4,5])
  putStrLn "\n-------------- mapM print [\"one\", \"two\", \"three\"] -----------"
  mapM print ["one", "two", "three"]
  putStrLn "\n-------------- mapM_ putChar ['a', 'b', 'c', '\\n'] -----------"
  mapM_ putChar ['a', 'b', 'c', '\n']
  putStrLn "\n-------------- forM print ['a', 'b', 'c', '\\n'] -----------"
  forM_ [Red', Green', Blue'] print 







  --sequence (map putStrLn [(show (yesno $ length [])), (show (yesno ("haha" :: [Char]))), (show (yesno $ Just 0))])








{-
  present2 (map putStrLn [(show (yesno $ length [])), (show (yesno ("haha" :: [Char]))), (show (yesno $ Just 0))])
present2 [a] = do
  sequence (map print a[1], a[2], a[3])
-}    

  -- Either print problems !!!
  --putStrLn $ show $ (Right 3.423 :: Either Double)   -- does not compile
  --putStrLn $ show $ (Right 3.423 :: Either a Double)   -- does not compile
  --putStrLn $ show $ (Left "Test" :: Either String) -- does not compile
  --putStrLn $ show $ (Left "Test" :: Either () String) -- does not compile
  putStrLn $ show $ (Right True :: Either () Bool)   -- working -- Right True
  putStrLn $ show $ (Right 3.423 :: Either () Double) -- working -- Right 3.423
  (putStrLn . show) (Right 3.423 :: Either () Double) -- working -- Right 3.423
-- some cool stuff
  putStrLn $ show $ sum' []
  putStrLn $ show $ length ("abcdef" :: String)           
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
-}

-- specShow   putStrLn "\n-----------------------------
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
specHeader  a | a /= "" = specHeader2 a | otherwise = return () -- from Control.Monad
  --Nothing -- print ""


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
id2 :: a  -> a
id2  = \x -> x

-- 2 arguments lambda notation
id4 :: Num a => a -> a -> a      -- does not compile, but works in ghci
id4 = \x y -> 2*x + y    

-- versions by Willem Van Onsem
id4'  x y = 2*x + y
id4''     = (+) . (2 *)



id5 = 1 + 2 :: Int

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


-- =============================== Algebraic Datatypes =======================
-- see Cards.hs too
-- data Car
data Car = Car  { company :: String
                , model :: String 
                , year :: Int               
                } deriving (Show, Read, Eq, Ord)
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

-- data Color'                               
data Color'  = Blue' | Green' | Red' deriving (Show, Read, Eq, Enum) 
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
-- =============================== Typeclasses ==========================
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

-- Point', using Tuples
type Point' = (Int, Int)
--
origin' :: Point'
origin' = (0, 0)
-- move a given point to the right
moveRight :: Point' -> Int -> Point'
moveRight (x, y) distance'  = (x + distance', y)
-- move a given point to upwards
moveUp :: Point' -> Int -> Point'
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

-- iterare (lambda notation)
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
-- variant 2
factorial2 :: (Integral a) => a -> a
factorial2 0 = 1
factorial2 n = n * factorial2 (n -1)
-- variant 3 (my own)
factorial3 :: (Integral a) => a -> a
factorial3 0 = 1
factorial3 n = product [(factorial3 0)..n]


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
-- factorial4
factorial4 :: (Integral a) => a -> a  
factorial4 0 = 1  
factorial4 n = n * factorial4 (n - 1)     -- recursion

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
charName '\NUL' = "Mr. Null"      -- same as '\0'
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

--tellVal :: [Char]
tellVal = tell ([] :: [()])

-- length - the original one
----length       :: [a] -> Int  -- not anymore, it is Foldable
--length       :: Foldable t => t a -> Int
--length []    =  0
--length (_:l) =  1 + length l

-- length' ---
--    This function replaces every element of a list with 1 and then sums that up
length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]  

-- length''   -- recursive version 
length'' :: (Num b) => [a] -> b  
length'' [] = 0  
length'' (_:xs) = 1 + length'' xs

-- sum        -- recursive version
sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs 

-- capital
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

-- all func           (with lambda notation)
--    returns True if all items in the list fulfill the condition
bValAll  = all (<10) [1,3,5,7,9]               -- True
bValAll2 = all even [2,4,6,8,10]               -- True
bValAll3 = all (\x -> (x*x)/4 > 10) [5,10,15]  -- False

-- @ -----

-- guards ------ see also signum
-- max'' --
max'' :: (Ord a) => a -> a -> a  
max'' a b   
    | a > b     = a  
    | otherwise = b 
--max'' a b | a > b = a | otherwise = b 

-- myCompare --
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT 

-- initials ---
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname 

-- bmiTell with gurds and where binding 
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where 
      bmi = weight / height ^ 2 
      --skinny = 18.5
      --normal = 25.0
      --fat    = 30.0
      (skinny, normal, fat) = (18.5, 25.0, 30.0)   -- pattern match here

-- calcBmis with guards and "where" binding --
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2
-- "let" without "in". 
--    here is a let inside a list comprehension much like we would a predicate, 
--    only it doesn't filter the list, it only binds to names. 
--    The names defined in a let inside a list comprehension are visible to 
--    the output function (the part before the |) and all predicates and sections 
--    that come after of the binding. 
--    So we could make our function return only the BMIs of fat people
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- =========================== let-in binding ==================================
--    Very similar to where bindings are let bindings. Where bindings are a syntactic construct 
--    that let you bind to variables at the end of a function and the whole function can see 
--    them, including all the guards. Let bindings let you bind to variables anywhere and are 
--    expressions themselves, but are very local, so they don't span across guards. 
--    Just like any construct in Haskell that is used to bind values to names, let bindings 
--    can be used for pattern matching. Let's see them in action! This is how we could define 
--    a function that gives us a cylinder's surface area based on its height and radius:      
----
-- cylinder with "let" binding (with "in") --
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

-- "let-in" vs "if-else-then"
resExp1 = 4 * (let a = 9 in a + 1) + 2
resExp2 = 4 * (if 10 > 5 then 10 else 0) + 2  

-- "let-in" and "where"
--  The difference is that "let" bindings are expressions themselves. 
--  "where" bindings are just syntactic constructs. Remember when we did the 
--  if statement and it was explained that an if else statement is an expression and 
--  you can cram it in almost anywhere?
--  The form is "let <bindings> in <expression>"
resExp3 = [let square x = x * x in (square 5, square 3, square 2)] 
resExp4 = (let (a,b,c) = (1,2,3) in a+b+c) * 100

-- case expressions --
{-
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...  
-}
-- describe list
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of []  -> "empty."
                                               [_] -> "a singleton list."
                                               xs  -> "a longer list."
-- describe list, but without case-of
describeList'    :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
    where what []  = "empty."  
          what [_] = "a singleton list."  
          what xs  = "a longer list."

-- describe list with guards
describeList2      :: [a] -> String  
describeList2 xs   = "The list is " ++ myList xs
myList :: [a] -> String
-- null :: Foldable t => t a -> Bool
myList xs
    | null          xs = "empty."
    | null (drop 1 xs) = "a singleton list."
    | otherwise        = "a longer list."

-- describe list with guards
describeList3    :: [a] -> String  
describeList3 xs = "The list is " ++ what xs
    where what xs 
              | []   <- xs = "empty."
              | [_]  <- xs = "a singleton list."
              | otherwise  = "a longer list."

-- describe list with guards
{-
describeList4    :: [a] -> String       - this one does not compile
describeList4 xs = "The list is " ++ 
              | [] <- xs = "empty." 
              | [_] <- xs = "a singleton list." 
              | otherwise = "a longer list."
-}

-- ==========================================
-- 4 versions of myList 
-- using case-of 
myList1    :: [a] -> String
myList1 xs = case xs of []  -> "empty"
                        [_] -> "one"
                        xs  -> "more"
-- using guards and patters
myList2 :: [a] -> String
myList2 xs
    | []  <- xs = "empty"
    | [_] <- xs = "one"
    | otherwise = "more"

-- using null :: Foldable f => f a -> Bool and drop :: Int -> [a] -> [a]
-- null :: Foldable t => t a -> Bool
myList3 :: [a] -> String
myList3 xs
    | null          xs = "empty"
    | null (drop 1 xs) = "one"
    | otherwise        = "more"

-- using where and patterns
myList4 :: [a] -> String
myList4 xs = what xs 
  where what []  = "empty"
        what [_] = "one"
        what xs  = "more"

-- anonimus function again (Lambda notation)
--   But here's another way, where we pass the anonymous function into map 
--   rather than any named function.
addOneList     :: Num b => [b] -> [b]
addOneList lst = map addOne' lst
    where addOne' x = x + 1

addOneList'    :: Num b => [b] -> [b]
addOneList' lst = map (\x -> x + 1) lst     -- lambda notation

--   For completeness it's worth mentioning that this could be better written using a section, 
--   in pointfree style:

addOneList'' :: [Integer] -> [Integer]
addOneList'' =  map (+1)


-- https://www.futurelearn.com/courses/functional-programming-haskell/0/steps/27226
-- © University of Glasgow 
{- guards notation
f x
  | predicate1 = expression1
  | predicate2 = expression2
  | predicate3 = expression3
-}
--example without guards
absolute1 x = if (x < 0) then (negate x) else x

--example with guards
absolute2 x 
  | x<0 = -x
  | otherwise = x

---------  
data Pet = Cat|Dog|Fish|Parrot String|Lizard|Bird|Hamster  deriving (Show)
hello :: Pet -> String
hello x = 
  case x of
    Cat         -> "meeow"
    Dog         -> "woof"
    Fish        -> "bubble"
    Parrot name -> "pretty " ++ name
    Lizard      -> "psss"
    _           -> "grunt"            -- this is catch-all pattern

-- N.B. begin - investigate this code later 
{-
-- replacing case-of with guards ---
firstFunction  :: String -> Maybe MyType
secondFunction :: MyType -> Integer
myFunction     :: String -> Maybe Integer
myFunction xs = case firstFunction xs of
    Nothing -> Nothing
    Just x  -> Just( secondFunction x )
-- 
myFunction1    :: String -> Maybe Integer
myFunction1 xs | Just x <- firstFunction xs = Just (secondFunction x)
               | otherwise = Nothing

-- fmap
myFunction2    :: String -> Maybe Integer
myFunction2 xs = fmap secondFunction (firstFunction xs)

-- fmap
fmap :: Functor f => (a -> b) -> f a -> f b  -- is used to "map" over a functor
instance Functor Maybe  where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)
-}

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

--allMultiples :: [Int, Int] -> [Int]
--allMultiples [a, b] = where

--  
--      let m = [1 .. ] 
--      let n = add1 n  
--a = 9
--x = take 10 (iterate (a*)1) 
-}

{-
----- @ --------------
someFunctio1 :: SomeType -> SomeType
someFunctio1 leaf@(Leaf _ _ _) = Leaf   -- same thing
someFunctio1 Nil = Leaf 0 0 0
---
someFunctio2 :: SomeType -> SomeType
someFunctio2 (Leaf x y z) = Leaf x y z  -- same thing
someFunctio2 Nil = Leaf 0 0 0
----
Besides the argument pattern matching usage described in the answer of @Sibi, 
in Haskell the "at" character ('@', also known as an arobase character) can be used 
in some contexts to force a typing decision. This is mentioned in the comments by @Josh.F.

This is not part of the default language features, and is known as 
the Type Application Haskell language extension. 
In summary, the extension allows you to give explicit type arguments to a 
polymorphic function such as read. In a classic .hs source file, 
the relevant pragma must be included:

-- {-#  LANGUAGE TypeApplications  #-}
example:
 λ> let x = (read @Integer "33")
<interactive>:12:10: error:
    Pattern syntax in expression context: read@Integer
    Did you mean to enable TypeApplications?
 λ> :set -XTypeApplications
 λ>
 λ> let x = (read @Integer "33")
 λ>
 λ> :t x
 x :: Integer
-}



-- Recursion ---------
xsMax1 = [(1,3),(4,3),(2,4),(6,2),(5,3),(5,6),(3,1),(6,1)]
xsMax2 = [1,3,4,3,2,4,5,3,5,6,3,1,6,1]
xsMax3 = "AbXdeF90zY"
xsMax4 = ["aBc","zY2","zz","zz2","abc","0","zza"]
xsMax5 = [("aBc","zY2"), ("zza","zz2"),("abc", "0"), ("zza", "zzz")]

-- maximum' --------
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where 
      maxTail = maximum' xs 
      
-- maximum'' --------
maximum'' :: (Ord a) => [a] -> a  
maximum'' [] = error "maximum of empty list"  
maximum'' [x] = x  
maximum'' (x:xs) = max x (maximum' xs)

-- replicate' -------
--    Note: Num is not a subclass of Ord. That means that what constitutes for a number 
--    doesn't really have to adhere to an ordering. So that's why we have to specify both 
--    the Num and Ord class constraints when doing addition or subtraction and also comparison.
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x

-- take' ----------
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs

-- drop' ----------
drop'                   :: Int -> [a] -> [a]  
drop' n xs     | n <= 0 =  xs  
drop' _ []              =  []  
drop' n (_:xs)          =  drop (n-1) xs

-- splitAt' --------
splitAt'                :: Int -> [a] -> ([a],[a])  
splitAt' n xs           =  (take' n xs, drop' n xs)    

-- reverse' --------
reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]

-- repeat' ---------
repeat' :: a -> [a]  
repeat' x = x:repeat' x

-- zip' ------------
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- elem' -----------
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs

-- elem2 with foldl ----
elem2 :: (Eq a) => a -> [a] -> Bool  
elem2 y ys = foldl (\acc x -> if x == y then True else acc) False ys  

-- elem 3, uring (right) foldr
elem3 :: (Eq a) => a -> [a] -> Bool  
elem3 y ys = foldr (\x acc -> if x == y then True else acc) False ys  

-- more foldr ---
--  http://zvon.org/other/haskell/Outputprelude/
--  it takes the second argument and the last item of the list and applies the function, 
--  then it takes the penultimate item from the end and the result, and so on. 
--  See scanr for intermediate results.
rsFdr1 = foldr (+) 5 [1,2,3,4]                   -- 15
rsFdr2 = foldr (/) 2 [8,12,24,4]                 -- 8.0
rsFdr3 = foldr (/) 3 []                          -- 3.0
rsFdr4 = foldr (&&) True [1>2,3>2,5==5]          -- False
rsFdr5 = foldr max 111 [3,6,12,4,55,11]          -- 111
rsFdr6 = foldr (\x y -> (x+y)/2) 54 [12,4,10,6]  -- 12.0
rsFdr7 = foldr (/) 64 [4,2,4]                    -- 0.125

-- compare with foldl
rsFdl1 = foldl (+) 5 [1,2,3,4]                   -- 15
rsFdl2 = foldl (/) 2 [8,12,24,4]                 -- 2.1701388888888888e-4
rsFdl3 = foldl (/) 3 []                          -- 3.0
rsFdl4 = foldl (&&) True [1>2,3>2,5==5]          -- False
rsFdl5 = foldl max 111 [3,6,12,4,55,11]          -- 111
rsFdl6 = foldl (\x y -> (x+y)/2) 54 [12,4,10,6]  -- 10.125
rsFdl7 = foldl (/) 64 [4,2,4]                    -- 2.0


-- quicksort -------------------
{-
--  Hoare's quick sort in C
void quicksort (int a[], int 1, int r)
{
int i = 1;
int j = r;
int x = a[(l + r) / 2];
do
{
while (a[i] < x) i++;
while (x < a[j]) j--;
if (i <= j)
{
int temp = a[i];
a[i++] = a[j] ;
a[j--] = temp;
}
}
while (i <= j);
if (1 < j) quicksort (a, 1, j);
if (i < r) quicksort (a, i, r);
}
-}

-- Hoare's quick sort in Haskell (by Dushkin)
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []
quicksort (x:xs) = quicksort [у | у <- xs, у < x] ++
                   [x] ++
                   quicksort [у | у <- xs, у >= x]

------- this one more readable ------------
quicksort' :: (Ord a) => [a] -> [a]  
quicksort' [] = []  
quicksort' (x:xs) =   
    let smallerSorted = quicksort' [a | a <- xs, a <= x]  
        biggerSorted = quicksort' [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted
--------------


rsQS1 = quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9] 
                              -- [1,2,2,3,3,4,4,5,6,7,8,9,10]
rsQS2 = quicksort "the quick brown fox jumps over the lazy dog" 
                              -- "        abcdeeefghhijklmnoooopqrrsttuuvwxyz"
rsQS3 = quicksort [10,2,5,3,1,6,-99,4,2,3,4.0,8,9] 
                              -- [-99.0,1.0,2.0,2.0,3.0,3.0,4.0,4.0,5.0,6.0,8.0,9.0,10.0]
rsQS4 = quicksort [(10,2,5),(3,1,6),(-99,4,2),(3,4.0,8),(-99,5,-100)] 
                              -- [(-99,4.0,2),(-99,5,-100),(3,1.0,6),(3,4.0,8),(10,2.0,5)]

-- curried functions ----------
resCur1 = max 4 5 
resCur2 = (max 4) 5

-- max :: (Ord a) => a -> a -> a
---
-- max :: (Ord a) => a -> (a -> a)
-- max takes an a and returns (that's the ->) a function that takes an a and returns an a

--multThree       :: (Num a) => a -> a -> a -> a
--multThree       :: (Num a) => a -> (a -> a -> a)
multThree       :: (Num a) => a -> (a -> (a -> a))  
multThree x y z = x * y * z
resCur3 = multThree 3 5 9       -- 135
resCur4 = ((multThree 3) 5) 9   -- 135
-- First, 3 is applied to multThree, because they're separated by a space. 
-- That creates a function that takes one parameter and returns a function. 
-- So then 5 is applied to that, which creates a function that will take a parameter 
-- and multiply it by 15. 9 is applied to that function and the result is 135

-- curry, using function with too few parameters, or partialy applyed function
-- By calling functions with too few parameters, we're creating new functions on the fly.
--multTwoWithNine :: Integer -> Integer -> Integer
multTwoWithNine :: (Num a) => a -> a -> a
multTwoWithNine = multThree 9   -- here is only 1 paremeter, but should be 3 parameters
resCur5 = multTwoWithNine 2 3   -- 54 (all together 3 parameters)

-- curry, using function with too few parameters, or partialy applyed function
-- another example of using function with less then expected parameters
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x      -- this is a regular implementaion
---
compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100         -- this one is with fewer parameters
-- about compare
-- it has a type  "compare :: (Ord a) => a -> a -> Ordering", and calling it with 100,
-- returns a "(Num a, Ord a) => a -> Ordering"
-- "compare 10 9" == "(compare 10) 9"

-- Infix functions can also be partially applied by using sections. 
-- To section an infix function, simply surround it with parentheses and 
-- only supply a parameter on one side. That creates a function that takes 
-- one parameter and then applies it to the side that's missing an operand
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10) 
-- "200 / 10" == "divideByTen 200" == "(/10) 200"

-- zipWith' ----------
zipWith'                 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- zipWith' using a function with a lambda Notation
zResVal0 = zipWith' (\x y -> 2*x + y) [1..4] [5..8]     -- [7,10,13,16]
zResVal1 = zipWith' (**) (replicate 10 5) [1..10]       
              -- [5.0,25.0,125.0,625.0,3125.0,15625.0,78125.0,390625.0,1953125.0,9765625.0]
zResVal2 = zipWith' (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
              -- [153.0,61.5,31.0,15.75,6.6]

-- lambda notation and equivalents
lmResVal1 = (\x y -> 2*x + y) 4 5   
lmResVal2 x y = 2*x + y             -- it is a function already
lmResVal3 = (+) . (2 * )

-- Intermission: gcd
-- gcd - greatest common divisor (наибольший общий делитель)
-- gcd :: Integral a => a -> a -> a
resVal94  = gcd 12 8           -- 4
resVal94' = gcd 12 (gcd 16 8)  -- 4
---- gcd implementation
gcd' :: Integral t => t -> t -> t
gcd' 0 y = y
gcd' x y = gcd' (y `mod` x) x
----
myGCD :: Integral t => t -> t -> t
myGCD x y | x < 0     = myGCD (-x) y
          | y < 0     = myGCD x (-y)
          | y < x     = gcd' y x
          | otherwise = gcd' x y
----
myGCD' :: Integer -> Integer -> Integer
myGCD' a b
      | b == 0     = abs a
      | otherwise  = myGCD' b (a `mod` b)

--- gcd with 3 and more -- algorithm
--gcd(a, b, c) = gcd(a, gcd(b, c)) = gcd(gcd(a, b), c) = gcd(gcd(a, c), b)
res3gcd1 = gcd' 12 $ gcd' 36 96 
res3gcd2 = gcd' (gcd' 12 36) 12 
res3gcd3 = gcd' (gcd' 12 96) 36 

---- gcd'' for 3 arguments
gcd''       :: Integral t => t -> t -> t -> t
gcd'' a b c = gcd' a $ gcd' b c 

---- lcm'  lowest common multipble, наименьшее общее частное
lcm' :: Integral a => a -> a -> a
--lcm' :: (Fractional a, Integral a) => a -> a -> a
lcm' _ 0    = 0
lcm' 0 _    = 0
lcm' a b    = abs ((a `quot` (gcd a b)) * b) 
-- lcm'' for 3 arguments
lcm''       :: Integral a => a -> a -> a -> a
lcm'' a b c = lcm' a $ lcm' b c 

-- Intermission:  Syntctic Sugar --------
-- https://wiki.haskell.org/Syntactic_sugar
-- x `elem` xs      is sugar for    elem x xs 
-- `elem` xs        is sugar for    flip elem xs      -- ??? does not look correct ?!
-- [1, 2, 3]        is sugar for    (1:2:3:[])
-- do x <- f; g x   is sugar for    f >>= (\x -> g x) -- N.B. think about this sample

-- flip - it evaluates the function flipping the order of arguments
--flip :: (a -> b -> c) -> b -> a -> c
resVal93 = flip (/) 1 2    -- 2
resVal92 = flip (>) 3 5    -- True
resVal91 = flip mod 3 6    -- 0

--- flip' with lampda, here it is OK, because you want to make it explicit that 
--    your function is mainly meant to be partially applied and passed on to a function 
--    as a parameter.
flip'   :: (a -> b -> c) -> b -> a -> c  
flip' f = \x y -> f y x 

--- ======================== monads, AKA computation builders ===========================
--  https://stackoverflow.com/questions/44965/what-is-a-monad

-- example 1. List comprehension
 -- list Comprehension
resMod1 = [x*2 | x<-[1..10], odd x]
-- do
resMod2 = do 
            x <- [1..10] 
            guard (odd x)          
            return (x * 2)
-- >>=
resMod3 = [1..10] >>= (\x -> guard (odd x) >> return (x*2))
---
-- example 2. Input/Output
resMod4 = do
            putStrLn "What is your name?"
            name <- getLine
            putStrLn ("Welcome, " ++ name ++ "!")

-- example 3. A parser
{-
parseExpr = parseString <|> parseNumber
parseString = do
        char '"'
        x <- many (noneOf "\"")
        char '"'
        return (StringValue x)
parseNumber = do
    num <- many1 digit
    return (NumberValue (read num))
-}

-- =================== List comprehension and List Monad ===========================
-- https://wiki.haskell.org/List_comprehension
resVal99 = [toUpper c | c <- (s :: String)]
    where 
      --s :: String
      s = "Hello"                   -- "HELLO"
resVal98 = [(i,j) | i <- [1,2], 
                    j <- [1..4] ]   -- [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4)]
resVal97 = take 10 [ (i,j) | i <- [1,2], 
                             j <- [1..] ] 
                -- [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10)]
resVal96 = take 5 [ [ (i,j) | i <- [1,2] ] | j <- [1..] ]
                -- [[(1,1),(2,1)], [(1,2),(2,2)], [(1,3),(2,3)], [(1,4),(2,4)], [(1,5),(2,5)]]
-- boolean guard
resVal95 = take 10 [ (i,j) | i <- [1..], 
                             j <- [1..i-1], 
                             gcd i j == 1 ]
                            -- [(2,1),(3,1),(3,2),(4,1),(4,3),(5,1),(5,2),(5,3),(5,4),(6,1)]

---- map ------------------
-- returns a list constructed by appling a function (the first argument) to all items 
-- in a list passed as the second argument
-- map :: (a -> b) -> [a] -> [b]
resMap1 = map abs [-1,-3,4,-12]                     -- [1,3,4,12]
resMap2 = map reverse ["abc","cda","1234"]          -- ["cba","adc","4321"]
resMap3 = map (3*) [1,2,3,4]                        -- [3,6,9,12]
resMap4 = map (recip . negate) [1,4,-5,0.1]         -- [-1.0,-0.25,0.2,-10.0]
resMap5 = map (print) [1,3,5,6,7]                   
          -- [<<IO action>>,<<IO action>>,<<IO action>>,<<IO action>>,<<IO action>>]
-- here is lambda func again                        
resMap6 = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]  -- [3,8,9,8,7]  

-- simple map ---
rsMps1 = map (+ 0) [1, 3, 3, 4] -- [1,3,3,4]
rsMps2 = map (* 0) [1, 3, 3, 4] -- [0,0,0,0]
rsMps3 = map (/ 1) [1, 3, 3, 4] -- [1.0,3.0,3.0,4.0]
rsMps4 = map (/ 0) [1, 3, 3, 4] -- [Infinity,Infinity,Infinity,Infinity]

--------------- intermission -----------------
-- recip ----
-- Fractional a => a -> a
-- returns 1 / argument
rsRcp1 = recip 0.1           -- 10.0
rsRcp2 = recip 4             -- 0.25

-- . -----
-- function composition. In math  (f o g)(x) = f(g(x))
-- (.)   :: (b -> c) -> (a -> b) -> a -> c  
-- f . g = \x -> f (g x) 
rsDot1 = (negate . abs) (-1)                  -- -1
rsDot2 = (reverse . take 10 . enumFrom) 10    --  [19,18,17,16,15,14,13,12,11,10]
rsDot3 = (abs . snd)(-1,-3)                   -- 3
rsDot4 = ((2+).(3*).(4-)) 2                   -- 8

--- compare, which one is clearer and more concise
rsLmb1 = map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  -- [-5,-3,-6,-7,-3,-2,-19,-24]
rsDot6 = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]          -- [-5,-3,-6,-7,-3,-2,-19,-24] 
---
rsLmb2 = map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]   -- [-14,-15,-27] 
rsDot7 = map (negate . sum . tail) [[1..5],[3..6],[1..7]]             -- [-14,-15,-27]

-- using (.) with functions having several parameters -------------
-- we have to partially apply them just so much that each function takes just one parameter
rsToMdf1 = sum (replicate 5 (max 6.7 8.9))          -- 44.5
-- modified versions
rsMdfyd1 = (sum.replicate 5 . max 6.7) 8.9          -- 44.5
rsMdfyd2 = sum . replicate 5 . max 6.7 $ 8.9        -- 44.5
---
rsToMdf2 = replicate 10 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
        -- [1632960,1632960,1632960,1632960,1632960,1632960,1632960,1632960,1632960,1632960]
rsMdfyd4 = replicate 10 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]
        -- [1632960,1632960,1632960,1632960,1632960,1632960,1632960,1632960,1632960,1632960]

------- point free style of function definitions ------------
-- Another common use of function composition is defining functions in the so-called 
-- point free style (also called the pointless style)
sum5    :: (Num a) => [a] -> a     
--sum5 xs = foldl (+) 0 xs      -- xs is exposed on both sides, because of curring we can omit xs
sum5 = foldl (+) 0              -- this version is written in point free style
---
fn :: (RealFrac a, Integral b, Floating a) => a -> b 
fn x = ceiling (negate (tan (cos (max 50 x))))
---
--fn' :: (RealFrac a, Integral b, Floating a) => a -> b 
fn' :: Double -> Integer
fn'  = ceiling . negate . tan . cos . max 50              -- point free style too

-- ceiling --- returns the least integer not less than argument
rsCeil1 = ceiling 3.000001      -- 4
rsCeil2 = ceiling 3             -- 3 
rsCeil3 = ceiling (-3.7)        -- -3

-- cos  --- cosine
-- cos :: Floating a => a -> a
rsCos1  = cos (pi/2)            -- 0.0
-- tan  --- tangent
-- tan :: Floating a => a -> a
rsTan1  = tan (pi/16)            -- 0.19891236737965798

-------------- another example of function composition -------------
--- regular function
oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))         -- 166650
---  point free style with function composition 
oddSquareSum' :: Integer  
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]      -- 166650
---  more readable version, which is better for people who will read this func
oddSquareSum'' :: Integer  
oddSquareSum'' =                                                              -- 166650
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit 


-- enumFrom ----
-- Enum a => a -> [a]
-- returns an array of members of an enumeration starting with the argument, 
-- it is equvalent to syntax.
rsEnFr1 = take 10 (enumFrom 'a')               -- "abcdefghij"
rsEnFr2 = take 10 (enumFrom 23)                -- [23,24,25,26,27,28,29,30,31,32]
-- small program with enumFrom
data XXX = AA|BB|CC|DD deriving (Enum, Show)
rsEnFr3 = enumFrom BB                          -- [BB,CC,DD]

--- map' using fordr -------
mapR      :: (a -> b) -> [a] -> [b]  
mapR f xs = foldr (\x acc -> f x : acc) [] xs

--- map' using fordl -------
mapL      :: (a -> b) -> [a] -> [b]  
--mapL :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a] -- automatic type created by ghc
mapL f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- Folds can be used to implement any function where you traverse a list once, 
-- element by element, and then return something based on that. 
-- Whenever you want to traverse a list to return something, chances are you want a fold. 
-- That's why folds are, along with maps and filters, 
-- one of the most useful types of functions in functional programming.

-- The foldl1 and foldr1 functions work much like foldl and foldr, 
-- only you don't need to provide them with an explicit starting value. 
-- They assume the first (or last) element of the list to be the starting value 
-- and then start the fold with the element next to it. 

-- Power of folds --- using folds for implementations
maximumF :: (Ord a) => [a] -> a  
maximumF = foldr1 (\x acc -> if x > acc then x else acc)  
---  
reverseF :: [a] -> [a]  
reverseF = foldl (\acc x -> x : acc) []  
--
reverseFr :: [a] -> [a]  
--reverseFr = foldr (\ x acc -> x : acc) []   -- this will not work (does not reverse)
--reverseFr = foldr (\acc x -> x : acc) []    -- this does not compile
reverseFr = foldl (flip (:)) []
--  
productF :: (Num a) => [a] -> a  
productF = foldr1 (*)  
---
productFl :: (Num a) => [a] -> a  
productFl = foldl1 (*)  
---  
filterF   :: (a -> Bool) -> [a] -> [a]  
filterF p = foldr (\x acc -> if p x then x : acc else acc) []  
---  
headF :: [a] -> a  
headF = foldr1 (\x _ -> x)   --head is better implemented by pattern matching
--
headF2 :: [a] -> a  
headF2 = foldl1 (\x _ -> x)  --head is better implemented by pattern matching
---  
lastF :: [a] -> a  
lastF = foldl1 (\_ x -> x)  
--
lastF2 :: [a] -> a  
lastF2 = foldr1 (\_ x -> x)  

--------------------- scanl, scanr, scanl1, scanr1 -----------------
-- scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator 
-- states in the form of a list. 
-- There are also scanl1 and scanr1, which are analogous to foldl1 and foldr1
-- Scans are used to monitor the progression of a function that can be implemented as a fold.
rsScL0 = scanl (+) 0 [1,2,3,4]           -- [0,1,3,6,10]
rsFlL0 = foldl (+) 0 [1,2,3,4]           -- 10
rsCcR0 = scanr (+) 0 [3,5,2,1]           -- [11,8,3,1,0]
rsFcR0 = foldr (+) 0 [3,5,2,1]           -- 11
---
rsScL2 = scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]  -- [3,4,5,5,7,9,9,9]
rsScL3 = scanl (flip (:)) [] [3,2,1]                                      -- [[],[3],[2,3],[1,2,3]]

-- Question: How many elements does it take for the sum of the roots of 
-- all natural numbers to exceed 1000? 
-- To get the squares of all natural numbers we just use map sqrt [1..]
-- Now we will use scan (to get result, we could use fold)
sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
---
rsSum1 = sum (map sqrt [1..130])  -- 993.6486803921487
rsSum2 = sum (map sqrt [1..131])  -- 1005.0942035344083

----------------- function application   $    ---------------
-- ($) :: (a -> b) -> a -> b  
-- f $ x = f x 
-- f a b c == ((f a) b) c 
-- f (g (z x)) == f $ g $ z x
-- sum (filter (> 10) (map (*2) [2..10]))   ==   sum $ filter (> 10) $ map (*2) [2..10]
-- =========================================================
-- But apart from getting rid of parentheses, $ means that function application can be treated
-- just like another function. That way, we can, for instance, map function application 
-- over a list of functions.  
-- ========================================================= !!!!!!!
rsMapS1 = map ($ 3) [(4+), (10*), (^2), sqrt]      -- [7.0,30.0,9.0,1.7320508075688772]  

-- 



-- addTree -----------
addThree :: (Num a) => a -> a -> a -> a  
addThree x y z = x + y + z
-- addTree with lambda notation
addThree' :: (Num a) => a -> a -> a -> a  
addThree' = \x -> \y -> \z -> x + y + z

--sum' :: (Num a) => [a] -> a               -- recursive version
--sum' [] = 0  
--sum' (x:xs) = x + sum' xs         

-- sum' --------------
sum''    :: (Num a) => [a] -> a  
sum'' xs = foldl (\acc x -> acc + x) 0 xs   -- using foldl
-- even better sum3
sum3 :: (Num a) => [a] -> a  
sum3 = foldl (+) 0                          -- using foldl, 
-- (\acc x -> acc + x) == (+) and we can omit xs as the parameter, 
-- because calling foldl (+) 0 will return a function that takes a list.
-- GENERALLY, IF we have a function, like "foo a = bar b a", we can
-- rewrite it like "foo = bar b"

-- sum func using foldl1 !!! --- does not work with empty lists
sum4 :: (Num a) => [a] -> a 
sum4 = foldl1 (+)

-- ========= (foo a = bar b a) == (foo = bar b) ========= !!!!!
fnAddTwo     :: Num a => a -> a -> a
fnAddTwo u t = u + t 
--
fnAddLong   :: Num a => a -> a
fnAddLong n = fnAddTwo 10 n       --  (foo a = bar b a)
-- or 
fnAddShort :: Num a => a -> a
fnAddShort = fnAddTwo 10          --  (foo = bar b), first argument of fnAdd3 passed to fnAddTen,
                                  --                 as a second parameter 
                                  --  fnAdd3 91.5 = 101.5

-- foldl ----
-- http://zvon.org/other/haskell/Outputprelude/
--  it takes the second argument and the first item of the list and applies the function to them, 
--  then feeds the function with this result and the second argument and so on. 
--  See scanl for intermediate results.
-- foldl2 :: (a -> b -> a) -> a -> [b] -> a
resFold1 = foldl (/) 64 [4,2,4]               -- 2.0
resFold2 = foldl (/) 3 []                     -- 3.0
resFold3 = foldl max 5 [1,2,3,4]              -- 5.0 
resFold4 = foldl max 5 [1,2,3,4,5,6,7]        -- 7
resFold5 = foldl (\x y -> 2*x + y) 4 [1,2,3]  -- 43

-- filter -----
filter2 :: (a -> Bool) -> [a] -> [a]  
filter2 _ [] = []  
filter2 p (x:xs)   
    | p x       = x : filter p xs  
    | otherwise = filter p xs

resFil1 = filter2 (>5) [1,2,3,4,5,6,7,8]                               -- [6,7,8]
resFil2 = filter2 odd [3,6,7,9,12,14]                                  -- [3,7,9]
resFil3 = filter2 (\x -> length (x :: String) > 4) ["aaaa","bbbbbbbbbbbbb","cc"] -- ["bbbbbbbbbbbbb"]
resFil4 = filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"  -- "uagameasadifeent"
resFil5 = filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"  -- "GAYBALLS" 

---- largestDivisible
largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0

-- takeWhile --
-- creates a list from another one, it inspects the original list and takes from it its elements 
-- to the moment when the condition fails, then it stops processing
reTW0 = takeWhile (<3) [1,2,3,4,5]                   -- [1,2]
reTW1 = takeWhile (>3) [1,2,3,4,5]                   -- [] 
reTW2 = takeWhile odd [1,3,5,7,9,10,11,13,15,17]     -- [1,3,5,7,9]
reTW3 = takeWhile (\x -> 6*x < 100) [1..20]          -- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
reTW4 = takeWhile ('w'>) "hello world"               -- "hello "

-- The sum of all odd squares that are smaller than 10,000. 
-- First, we'll begin by mapping the (^2) function to the infinite list [1..]. 
-- Then we filter them so we only get the odd ones. And then, we'll take elements from that list 
-- while they are smaller than 10,000. Finally, we'll get the sum of that list. 
reTW5 = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  -- 166650
-- the same with list comprehentions
reLC1 = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])  -- 166650  

-- =============================================
-- Collatz sequences ----
-- We take a natural number. If that number is even, we divide it by two. If it's odd, 
-- we multiply it by 3 and then add 1 to that. We take the resulting number 
-- and apply the same thing to it, which produces a new number and so on.
-- In essence, we get a chain of numbers. It is thought that for all starting numbers, 
-- the chains finish at the number 1. So if we take the starting number 13, we get 
-- this sequence: 13, 40, 20, 10, 5, 16, 8, 4, 2, 1. 13*3 + 1 equals 40. 40 divided by 2 is 20, etc.
-- We see that the chain has 10 terms.
-- Now what we want to know is this: for all starting numbers between 1 and 100, 
-- how many chains have a length greater than 15?
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)

rsChn1 = chain 10     -- [10,5,16,8,4,2,1]
rsChn2 = chain 1      -- [1]
rsChn3 = chain 30     -- [30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]
-- this function aswers our question
numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))       -- 86 -- answer
    where isLong xs = length xs > 15
--- N.B. Note: This function has a type of numLongChains :: Int because length returns an Int 
--  instead of a Num a for historical reasons. If we wanted to return a more general Num a, 
--  we could have used fromIntegral on the resulting length.

-- lambda version of isLong function
numLongChains' :: Int  
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100])) 

-- some trics using map 
rsMap7 = map (*) [0..]         -- this one has type                 rsMap7 :: [Integer -> Integer] 
rsMap8 = (rsMap7 !! 4) 5       -- 20      0, 5, 10, 15, 20 ..., but this one     rsMap8 :: Integer
rsMap9 = ((map (*) [0..]) !! 4) 5      -- 20  

-- ============================== import Data.List ========================
--import Data.List
--import Data.List (nub, sort)   -- if we want only these functions
--import Data.List hiding (nub)  -- if we want everything, except of nub
--import qualified Data.Map      -- if we want call funcs like "Data.Map.nub"
--import qualified Data.Map as M -- if we want call funcs like "M.nub"  
---
numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub               -- == \xs -> length (nub xs)

--- nub ---
--    The nub function removes duplicate elements from a list. In particular, it keeps only 
--    the first occurrence of each element. (The name nub means `essence'.) It is a special case of nubBy, 
--    which allows the programmer to supply their own equality test.
rsNub1 = nub [1,2,3,4,3,2,1,2,4,3,5]            -- [1,2,3,4,5]
rsNub2 = nub "asdfsadsa"                        -- "asdf"
rsNub3 = nub [1,2.0,3,4,3,2,1,2,4,3,5]          -- [1.0,2.0,3.0,4.0,5.0]

--- intersperse --- takes an element and a list and then puts that element in between 
--    each pair of elements in the list
rsDtL1 = intersperse '.' "MONKEY"                         -- "M.O.N.K.E.Y" 
rsDtL2 = intersperse 0 [1,2,3,4,5,6]                      -- [1,0,2,0,3,0,4,0,5,0,6]

--- intercalate --- takes a list of lists and a list. It then inserts that list in between 
--    all those lists and then flattens the result
rsDtL3 = intercalate " " ["hey","there","guys"]           -- "hey there guys"  
rsDtL4 = intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]    -- [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]

--- transpose transposes a list of lists. If you look at a list of lists as a 2D matrix, 
--    the columns become the rows and vice versa
rsDtL5 = transpose [[1,2,3],[4,5,6],[7,8,9]]              -- [[1,4,7],[2,5,8],[3,6,9]]
rsDtL6 = transpose ["hey","there","guys"]                 -- ["htg","ehu","yey","rs","e"]
--- exsample --- Say we have the polynomials 3x2 + 5x + 9, 10x3 + 9 and 8x3 + 5x2 + x - 1 and 
--    we want to add them together. We can use the lists [0,3,5,9], [10,0,0,9] and [8,5,1,-1] 
--    to represent them in Haskell. Now, to add them, all we have to do is this:
rsDtL7 = map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]  -- [18,8,6,17] 

--- foldl' and foldr' -- are stricter versions of their respective lazy incarnations. 
--    When using lazy folds on really big lists, you might often get a stack overflow error. 
--    The culprit for that is that due to the lazy nature of the folds, the accumulator value 
--    isn't actually updated as the folding happens. What actually happens is that the accumulator 
--    kind of makes a promise that it will compute its value when asked to actually produce 
--    the result (also called a thunk). That happens for every intermediate accumulator and all those 
--    thunks overflow your stack. The strict folds aren't lazy buggers and actually compute 
--    the intermediate values as they go along instead of filling up your stack with thunks. 
--    So if you ever get stack overflow errors when doing lazy folds, try switching to their strict versions.

--- concat --- flattens a list of lists into just a list of elements  
rsDtL8 = concat ["foo","bar","car"]               -- "foobarcar"  
rsDtL9 = concat [[3,4,5],[2,3,4],[2,1,1]]         -- [3,4,5,2,3,4,2,1,1]  

--- concatMap --- is the same as first mapping a function to a list and then concatenating the 
--    list with concat 
rsDtL10 = concatMap (replicate 4) [1..3]          -- [1,1,1,1,2,2,2,2,3,3,3,3]
-- concatMap (replicate 4) [1..3]         ==      concat ( map (replicate 4) [1..3])    

--- and --- takes a list of boolean values and returns True only if all the values in the list are True
rsDtL11 = and $ map (>4) [5,6,7,8]        -- True  
rsDtL12 = and $ map (==4) [4,4,4,3,4]     -- False  

--- or --- it returns True if any of the boolean values in a list is True
rsDtL13 = or $ map (==4) [2,3,4,5,6,1]    -- True  
rsDtL14 = or $ map (>4) [1,2,3]           -- False  

--- any and all take a predicate and then check if any or all the elements in a list satisfy 
--    the predicate, respectively. Usually we use these two functions instead of 
--    mapping over a list and then doing and or or
rsDtL15 = any (==4) [2,3,5,6,1,4]                               -- True  
rsDtL16 = all (>4) [6,9,10]                                     -- True  
rsDtL17 = all (`elem` ['A'..'Z']) ("HEYGUYSwhatsup" :: [Char])  -- False  
rsDtL18 = all (`elem` ['A'..'Z']) ['H','E','Y','G','U','Y','S','w','h','a','t','s','u','p']  -- False  
rsDtL19 = any (`elem` ['A'..'Z']) ("HEYGUYSwhatsup" :: [Char])  -- True  
--------- intermission ------
--rsDtL20 = 'o' `elem` "Zvon" -- does not complile, because (elem :: (Foldable t, Eq a) => a -> t a -> Bool)
-- but this works
rsDtL21 = 'o' `elem` ("Zvon" :: [Char])  
--- or this works too
rsDtL22 = 'o' `elem` ['Z','v','o','n']
rsDtL23 = elem 'o' ("aSdlkfjo"::[Char]) -- this compiles and works too

--- iterate --- takes a function and a starting value. It applies the function to the starting value, 
--    then it applies that function to the result, then it applies the function to that result again, etc. 
--    It returns all the results in the form of an infinite list.
rsDtL24 = take 10 $ iterate (*2) 1  -- [1,2,4,8,16,32,64,128,256,512]  
rsDtL25 = take 3 $ iterate (++ "haha") "haha"  -- ["haha","hahahaha","hahahahahaha"]

--- splitAt --- takes a number and a list. It then splits the list at that many elements, 
--    returning the resulting two lists in a tuple.
rsDtL29 = splitAt 3 "heyman"                        -- ("hey","man")  
rsDtL26 = splitAt 100 "heyman"                      -- ("heyman","")  
rsDtL27 = splitAt (-3) "heyman"                     -- ("","heyman")  
rsDtL28 = let (a,b) = splitAt 3 "foobar" in b ++ a  -- "barfoo"  
rsDtL28' = do b ++ a where (a,b) = splitAt 3 "foobar"  -- "barfoo"  
            
--- takeWhile --- is a really useful little function. It takes elements from a list 
--     while the predicate holds and then when an element is encountered that doesn't satisfy 
--     the predicate, it's cut off. It turns out this is very useful.
rsDtL30 = takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]  -- [6,5,4]  
rsDtL31 = takeWhile (/=' ') "This is a sentence"        -- "This"  
--- example --- Say we wanted to know the sum of all third powers that are under 10,000. 
--    We can't map (^3) to [1..], apply a filter and then try to sum that up because filtering 
--    an infinite list never finishes. You may know that all the elements here are ascending 
--    but Haskell doesn't. That's why we can do this:
rsDtL32 = sum $ takeWhile (<10000) $ map (^3) [1..]     -- 53361  

--- dropWhile --- is similar, only it drops all the elements while the predicate is true. 
--    Once predicate equates to False, it returns the rest of the list. 
--    An extremely useful and lovely function!
rsDtL33 = dropWhile (/=' ') "This is a sentence"        -- " is a sentence"  
rsDtL34 = dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]        -- [3,4,5,4,3,2,1] 
--- example --- We're given a list that represents the value of a stock by date. 
--    The list is made of tuples whose first component is the stock1 value, the second is the year, 
--    the third is the month and the fourth is the date. We want to know when the stock1 value 
--    first exceeded one thousand dollars!
--------------------------------------------------------------------
-- this is unsafe version of stock1 implementation
stock1  = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]  
rsDtL35 = head (dropWhile (\(val,y,m,d) -> val < 1000) stock1)  -- (1001.4,2008,9,4) 
--- see below the safe implementation, using "find" ----------------

--- span --- is kind of like takeWhile, only it returns a pair of lists. 
--    The first list contains everything the resulting list from takeWhile would contain 
--    if it were called with the same predicate and the same list. The second list contains 
--    the part of the list that would have been dropped.
rsDtL38 = let (fw, rest) = span (/=' ') "This is a sentence" 
            in "First word:" ++ fw ++ ", the rest:" ++ rest  -- "First word: This, the rest: is a sentence"  
rsDtL38' = span (/=' ') "This is a sentence"
--- break --- Whereas span spans the list while the predicate is true, break breaks it 
--    when the predicate is first true. Doing break p is the equivalent of doing span (not . p).
rsDtL36  = break (==4) [1,2,3,4,5,6,7]        -- ([1,2,3],[4,5,6,7])  
rsDtL37' = span (not . (==4)) [1,2,3,4,5,6,7] -- ([1,2,3],[4,5,6,7])  
rsDtL37  = span (/=4) [1,2,3,4,5,6,7]         -- ([1,2,3],[4,5,6,7])  
--    When using break, the second list in the result will start with the first element 
--    that satisfies the predicate.

--- sort --- simply sorts a list. The type of the elements in the list has to be 
--      part of the Ord typeclass, because if the elements of a list can't be put 
--      in some kind of order, then the list can't be sorted.
rsDtL39 = sort [8,5,3,2,1,6,4,2]            -- [1,2,2,3,4,5,6,8]  
rsDtL40 = sort "This will be sorted soon"   -- "    Tbdeehiillnooorssstw" 

--- group --- takes a list and groups adjacent elements into sublists if they are equal.
rsDtL41 = group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]   -- [[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
-- If we sort a list before grouping it, we can find out how many times each element appears in the list.
rsDtL42 = map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] 
                                                    -- [(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]

--- ======================== find out about "@" later ===========================

--- inits --- tails --- inits and tails are like init and tail, only they recursively apply that 
--      to a list until there's nothing left. Observe.
rsDtL43 = inits "w00t"                    -- ["","w","w0","w00","w00t"]  
rsDtL44 = tails "w00t"                     -- ["w00t","00t","0t","t",""]  
rsDtL45 = let w = "w00t" in zip (inits w) (tails w)  
                                          -- [("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]

--- example --- Let's use a fold to implement searching a list for a sublist.
search :: (Eq a) => [a] -> [a] -> Bool  
search needle haystack =   
    let nlen = length needle  
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
-- First we call tails with the list in which we're searching. Then we go over each tail and see 
--      if it starts with what we're looking for. 
--      With that, we actually just made a function that behaves like isInfixOf. 
rsDtL46' = search "cat" "im a cat burglar"
--- isInfixOf --- searches for a sublist within a list and returns True if the sublist we're 
--      looking for is somewhere inside the target list.
rsDtL46 = "cat" `isInfixOf` "im a cat burglar"  -- True  
rsDtL47 = "Cat" `isInfixOf` "im a cat burglar"  -- False  
rsDtL48 = "cats" `isInfixOf` "im a cat burglar" -- False  

--- isPrefixOf --- isSuffixOf --- isPrefixOf and isSuffixOf search for a sublist at the beginning 
--      and at the end of a list, respectively.
rsDtL49 = "hey" `isPrefixOf` "hey there!"         -- True  
rsDtL50 = "hey" `isPrefixOf` "oh hey there!"      -- False  
rsDtL51 = "there!" `isSuffixOf` "oh hey there!"   -- True  
rsDtL52 = "there!" `isSuffixOf` "oh hey there"    -- False  

--- elem --- notElem --- check if an element is or isn't inside a list
--- partition --- takes a predicate and a list and returns a pair of lists. The first list in 
--      the result contains all the elements that satisfy the predicate, the second contains 
--      all the ones that don't.
rsDtL53 = partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"   -- ("BOBMORGAN","sidneyeddy")  
rsDtL54 = partition (>3) [1,3,5,6,3,2,1,0,3,7]                  -- ([5,6,7],[1,3,3,2,1,0,3])
rsDtL53' = partition (`notElem` ['A'..'Z']) "BOBsidneyMORGANeddy"   -- ("BOBMORGAN","sidneyeddy")  
-- It's important to understand how this is different from span and break:
rsDtL55 = span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"        -- ("BOB","sidneyMORGANeddy") 
rsDtL56 = break (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"       -- ("","BOBsidneyMORGANeddy") 

--- ========================================================= --- 
--- find --- find takes a list and a predicate and returns the first element that satisfies the predicate.
--      But it returns that element wrapped in a Maybe value. We'll be covering algebraic data types more
--      in depth in the next chapter but for now, this is what you need to know: a Maybe value can either
--      be Just something or Nothing. Much like a list can be either an empty list or a list with some 
--      elements, a Maybe value can be either no elements or a single element. And like the type of 
--      a list of, say, integers is [Int], the type of maybe having an integer is Maybe Int. 
--      Anyway, let's take our find function for a spin.
rsDtL57 = find (>4) [1,2,3,4,5,6]                     -- Just 5               (Just 5 :: Num a => Maybe a)  
rsDtL58 = find (>9) [1,2,3,4,5,6]                     -- Nothing              (Nothing :: Maybe a)
-- find :: (a -> Bool) -> [a] -> Maybe a                                      (Just :: a -> Maybe a)
-- Notice the type of find. Its result is Maybe a. That's kind of like having the type of [a], 
-- only a value of the type Maybe can contain either no elements or one element, whereas a list 
-- can contain no elements, one element or several elements.

-------------------------------------------------
--- safe implementation of stock1, using find ---
--stock1  = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]  
--rsDtL35 = head (dropWhile (\(val,y,m,d) -> val < 1000) stock1)  -- (1001.4,2008,9,4)
rsDtL59 = find (\(val,y,m,d) -> val > 1000) stock1  -- Just (1001.4,2008,9,4)

--- elemIndex --- elemIndex is kind of like elem, only it doesn't return a boolean value. 
--      It maybe returns the index of the element we're looking for. If that element isn't in our list, 
--      it returns a Nothing.
--ghci> :t elemIndex  
--elemIndex :: (Eq a) => a -> [a] -> Maybe Int  
rsDtL60 = 4 `elemIndex` [1,2,3,4,5,6]               -- Just 3  
rsDtL61 = 10 `elemIndex` [1,2,3,4,5,6]              -- Nothing  

--- elemIndices --- elemIndices is like elemIndex, only it returns a list of indices, 
--      in case the element we're looking for crops up in our list several times. 
--      Because we're using a list to represent the indices, we don't need a Maybe type, 
--      because failure can be represented as the empty list, which is very much synonymous to Nothing
rsDtL62 = ' ' `elemIndices` "Where are the spaces?" -- [5,9,13]

--- findIndex --- findIndices --- findIndex is like find, but it maybe returns the index of the first 
--      element that satisfies the predicate. findIndices returns the indices of all elements that 
--      satisfy the predicate in the form of a list.
rsDtL63 = findIndex (==4) [5,3,2,1,6,4]                             -- Just 5  
rsDtL64 = findIndex (==7) [5,3,2,1,6,4]                             -- Nothing  
rsDtL65 = findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"     -- [0,6,10,14]

--- zip --- zipWith ---  up to 7 
rsDtL66 = zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]  -- [7,9,8]  
rsDtL67 = zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]                      -- [(2,2,5,2),(3,2,5,2),(3,2,3,2)]

--- lines --- unlines --- words --- unwords --- is a useful function when dealing with files or input 
--      from somewhere. It takes a string and returns every line of that string in a separate list
--      unlines is the inverse function of lines. It takes a list of strings and joins 
--      them together using a '\n'. words and unwords are for splitting a line of text into words 
--      or joining a list of words into a text. Very useful.
rsDtL68 = lines "first line\nsecond line\nthird line"         -- ["first line","second line","third line"]
rsDtL69 = unlines ["first line", "second line", "third line"] -- "first line\nsecond line\nthird line\n" 
rsDtL70 = words "hey these are the words in this sentence"    
                                          -- ["hey","these","are","the","words","in","this","sentence"]  
rsDtL71 = words "hey these           are    the words in this\nsentence"  
                                          -- ["hey","these","are","the","words","in","this","sentence"]  
rsDtL72 = unwords ["hey","there","mate"]  -- "hey there mate"

--- delete --- takes an element and a list and deletes the first occurence of that element in the list.
rsDtL73 = delete 'h' "hey there ghang!"                               -- "ey there ghang!"  
rsDtL74 = delete 'h' . delete 'h' $ "hey there ghang!"                -- "ey tere ghang!"  
rsDtL75 = delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"   -- "ey tere gang!" 

--- \\ --- "\\" is the list difference function. It acts like a set difference, basically. For every 
--      element in the right-hand list, it removes a matching element in the left one
rsDtL76 = [1..10] \\ [2,5,9]        -- [1,3,4,6,7,8,10]  
rsDtL77 = "Im a big baby" \\ "big"  -- "Im a  baby"
-- Doing [1..10] \\ [2,5,9] is like doing delete 2 . delete 5 . delete 9 $ [1..10].

--- union --- also acts like a function on sets. It returns the union of two lists. It pretty much goes 
--      over every element in the second list and appends it to the first one if it isn't already in yet. 
--      Watch out though, duplicates are removed from the second list!
rsDtL78 = "hey man" `union` "man what's up"         -- "hey manwt'sup" 
rsDtL79 = "hey man"
rsDtL80 = "man what's up"
rsDtL81 = rsDtL79 `union` rsDtL80                   -- "hey manwt'sup"
---
rsDtL82 = [1..7] `union` [5..10]                    -- [1,2,3,4,5,6,7,8,9,10]

--- intersect --- works like set intersection. It returns only the elements that are found in both lists.
rsDtL83 = [1..7] `intersect` [5..10]              -- [5,6,7] 

--- insert --- takes an element and a list of elements that can be sorted and inserts it into the last 
--      position where it's still less than or equal to the next element. In other words, insert will 
--      start at the beginning of the list and then keep going until it finds an element that's equal 
--      to or greater than the element that we're inserting and it will insert it just before the element.
rsDtL84 = insert 4 [3,5,1,2,8,2]                  -- [3,4,5,1,2,8,2]  
rsDtL85 = insert 4 [1,3,4,4,1]                    -- [1,3,4,4,4,1]
--      The 4 is inserted right after the 3 and before the 5 in the first example and in between the 3 
--      and 4 in the second example.
--      If we use insert to insert into a sorted list, the resulting list will be kept sorted.
rsDtL86 = insert 4 [1,2,3,5,6,7]                  -- [1,2,3,4,5,6,7]  
rsDtL87 = insert 'g' $ ['a'..'f'] ++ ['h'..'z']   -- "abcdefghijklmnopqrstuvwxyz"  
rsDtL88 = insert 3 [1,2,4,3,2,1]                  -- [1,2,3,4,3,2,1]

-- !!! ================================================================================= !!! --
--      What "length", "take", "drop", "splitAt", "!!" and "replicate" have in common is that they 
--      take an Int as one of their parameters (or return an Int), even though they could be more generic
--      and usable if they just took any type that's part of the Integral or Num typeclasses 
--      (depending on the functions). They do that for historical reasons. However, fixing that would 
--      probably break a lot of existing code. That's why Data.List has their more generic equivalents, 
--      named genericLength, genericTake, genericDrop, genericSplitAt, genericIndex and genericReplicate
--      N.B. "let xs = [1..6] in sum xs / length xs", we get a type error, because you can't 
--      use / with an Int. But "let xs = [1..6] in sum xs / genericLength xs" works out just fine
-- !!! ================================================================================= !!! --

--- genericLength --- genericTake --- genericDrop --- genericSplitAt --- genericIndex --- genericReplicate 
--      length :: Foldable t => t a -> Int
--      genericLength :: Num i => [a] -> i

--- nubBy --- deleteBy --- unionBy --- intersectBy --- groupBy --- 
--      The nub, delete, union, intersect and group functions all have their more general counterparts 
--      called nubBy, deleteBy, unionBy, intersectBy and groupBy. The difference between them is that the
--      first set of functions use == to test for equality, whereas the By ones also take an equality 
--      function and then compare them by using that equality function. group is the same as groupBy (==).

--      For instance, say we have a list that describes the value of a function for every second. We want 
--      to segment it into sublists based on when the value was below zero and when it went above. If we 
--      just did a normal group, it would just group the equal adjacent values together. But what we want
--      is to group them by whether they are negative or not. That's where groupBy comes in! The equality
--      function supplied to the By functions should take two elements of the same type and return True 
--      if it considers them equal by its standards.
rsDtL89 = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]  
rsDtL90 = groupBy (\x y -> (x > 0) == (y > 0)) rsDtL89 
                                  -- [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
--      From this, we clearly see which sections are positive and which are negative. The equality 
--      function supplied takes two elements and then returns True only if they're both negative or if 
--      they're both positive. This equality function can also be written as
--      "\x y -> (x > 0) && (y > 0) || (x <= 0) && (y <= 0)", although I think the first way is more readable.

---- intermission --------   "on" --- function from Data.Function.
--      on :: (b -> b -> c) -> (a -> b) -> a -> a -> c  
--      f `on` g = \x y -> f (g x) (g y) 

--      So doing (==) `on` (> 0) returns an equality function that looks like 
--      "\x y -> (x > 0) == (y > 0)". on is used a lot with the By functions because with it, we can do
rsDtL91 = groupBy ((==) `on` (> 0)) rsDtL89  
                                  -- [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
--      You can read it out loud: Group this by equality on whether the elements are greater than zero

--- sortBy --- insertBy --- maximumBy --- minimumBy ---
--      Similarly, the sort, insert, maximum and minimum also have their more general equivalents. 
--      Functions like groupBy take a function that determines when two elements are equal. 
--      sortBy, insertBy, maximumBy and minimumBy take a function that determine if one element is 
--      greater, smaller or equal to the other. The type signature of sortBy is 
--      "sortBy :: (a -> a -> Ordering) -> [a] -> [a]". If you remember from before, 
--      the Ordering type can have a value of LT, EQ or GT. sort is the equivalent of sortBy compare, 
--      because compare just takes two elements whose type is in the Ord typeclass and returns 
--      their ordering relationship.   

-- example -- Lists can be compared, but when they are, they are compared lexicographically. 
--      What if we have a list of lists and we want to sort it not based on the inner lists' contents 
--      but on their lengths? Well, as you've probably guessed, we'll use the sortBy function.
rsDtL92 = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]  
rsDtL93 = sortBy (compare `on` length) rsDtL92           -- [[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]
--      compare `on` length ... that reads almost like real English!
--      "compare `on` length" here is the equivalent of "\x y -> length x `compare` length y"


-- ============================== import Data.Char ========================
---- All these predicates have a type signature of "Char -> Bool"  
--- isControl checks whether a character is a control character.
--- isSpace checks whether a character is a white-space characters. That includes spaces, tab characters, 
--      newlines, etc.
--- isLower checks whether a character is lower-cased.
--- isUpper checks whether a character is upper-cased.
--- isAlpha checks whether a character is a letter.
--- isAlphaNum checks whether a character is a letter or a number.
--- isPrint checks whether a character is printable. Control characters, for instance, are not printable.
--- isDigit checks whether a character is a digit.
--- isOctDigit checks whether a character is an octal digit.
--- isHexDigit checks whether a character is a hex digit.
--- isLetter checks whether a character is a letter.
--- isMark checks for Unicode mark characters. Those are characters that combine with preceding letters 
--    to form latters with accents. Use this if you are French.
--- isNumber checks whether a character is numeric.
--- isPunctuation checks whether a character is punctuation.
--- isSymbol checks whether a character is a fancy mathematical or currency symbol.
--- isSeparator checks for Unicode spaces and separators.
--- isAscii checks whether a character falls into the first 128 characters of the Unicode character set.
--- isLatin1 checks whether a character falls into the first 256 characters of Unicode.
--- isAsciiUpper checks whether a character is ASCII and upper-case.
--- isAsciiLower checks whether a character is ASCII and lower-case.
---
rsDtCh1 = all isAlphaNum ("bobby283" :: [Char])           -- True  
rsDtCh2 = all isAlphaNum ("eddy the fish!" :: String)     -- False   
rsDtCh3 = words "hey guys its me"                         -- ["hey","guys","its","me"]  
rsDtCh4 = groupBy ((==) `on` isSpace) "hey guys its me"   -- ["hey"," ","guys"," ","its"," ","me"] 
rsDtCh5 = filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"  
                                                          -- ["hey","guys","its","me"]
--- generalCategory --- it has a type 
--     "generalCategory :: Char -> GeneralCategory"
rsDtCh6 = generalCategory ' '  -- Space  
rsDtCh7 = generalCategory 'A'  -- UppercaseLetter  
rsDtCh8 = generalCategory 'a'  -- LowercaseLetter  
rsDtCh9 = generalCategory '.'  -- OtherPunctuation  
rsDtCh10 = generalCategory '9'  -- DecimalNumber  
rsDtCh11 = map generalCategory " \t\nA9?|"  
                    -- [Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]
--- example ---
someChar1 = ' ' 
rsDtCh13 = generalCategory someChar1 == Space             -- True

{-
:i GeneralCategory
data GeneralCategory
  = UppercaseLetter
  | LowercaseLetter
  | TitlecaseLetter
  | ModifierLetter
  | OtherLetter
  | NonSpacingMark
  | SpacingCombiningMark
  | EnclosingMark
  | DecimalNumber
  | LetterNumber
  | OtherNumber
  | ConnectorPunctuation
  | DashPunctuation
  | OpenPunctuation
  | ClosePunctuation
  | InitialQuote
  | FinalQuote
  | OtherPunctuation
  | MathSymbol
  | CurrencySymbol
  | ModifierSymbol
  | OtherSymbol
  | Space
  | LineSeparator
  | ParagraphSeparator
  | Control
  | Format
  | Surrogate
  | PrivateUse
  | NotAssigned
        -- Defined in ‘GHC.Unicode’
instance Eq GeneralCategory -- Defined in ‘GHC.Unicode’
instance Ord GeneralCategory -- Defined in ‘GHC.Unicode’
instance Enum GeneralCategory -- Defined in ‘GHC.Unicode’
instance Show GeneralCategory -- Defined in ‘GHC.Unicode’
instance Bounded GeneralCategory -- Defined in ‘GHC.Unicode’
instance Read GeneralCategory -- Defined in ‘GHC.Read’
-}
--------------------------------------------------------
--- toUpper --- converts a character to upper-case. Spaces, numbers, and the like remain unchanged.
--- toLower --- converts a character to lower-case.
--- toTitle --- converts a character to title-case. For most characters, title-case is the same 
--      as upper-case.
--- digitToInt --- converts a character to an Int. To succeed, the character must be 
--      in the ranges '0'..'9', 'a'..'f' or 'A'..'F'.
rsDtCh14 = map digitToInt "34538cF"       -- [3,4,5,3,8]  
rsDtCh15 = map digitToInt "FF85AB"      -- [15,15,8,5,10,11]

--- intToDigit --- is the inverse function of digitToInt. It takes an Int in the range 
--      of 0..15 and converts it to a lower-case character
rsDtCh16 = intToDigit 15                -- 'f'  
rsDtCh17 = intToDigit 5                 -- '5'  

--- examples ---
--rsDtCh38 = toUpper ("abcdef 12345 !@#$%^&*()_+ ZXCVB абвгд ЯШЕРТ") 
rsDtCh39 = toUpper 'a'                  -- 'A'
rsDtCh40 = toUpper '5'                  -- '5' 
rsDtCh41 = toUpper 'λ'                  -- '\923'
rsDtCh42 = toUpper 'б'                  -- '\1041'
--- using Unicode version
rsDtCh43 = U.toUpper 'λ'                  -- '\923'
rsDtCh44 = U.toUpper 'б'                  -- '\1041'

--(∧) ∷ 𝔹 → 𝔹 → 𝔹  -- some cool Unicode symbols
rsDtCh45 = '𝔹'                            -- '\120121'

--- ord --- chr --- The ord and chr functions convert characters to their corresponding 
--      numbers and vice versa
rsDtCh18 = ord 'a'                      -- 97  
rsDtCh19 = chr 97                       -- 'a'  
rsDtCh20 = map ord "abcdefghλж哈"       -- [97,98,99,100,101,102,103,104,955,1078,21704]
--- The difference between the ord values of two characters is equal to how far apart they 
--      are in the Unicode table

--- example --- The Caesar cipher is a primitive method of encoding messages by shifting 
--      each character in them by a fixed number of positions in the alphabet. 
--      We can easily create a sort of Caesar cipher of our own, only we won't constrict 
--      ourselves to the alphabet
encode :: Int -> String -> String  
encode shift msg = 
    let ords = map ord msg  
        shifted = map (+ shift) ords  
    in  map chr shifted 

-- same functionality, but using composition
encode' :: Int -> String -> String  
encode' shift msg =  map (chr . (+ shift) . ord) msg 
---
rsDtCh21 = encode 3 "Heeeeey"                       -- "Khhhhh|"  
rsDtCh22 = encode 4 "Heeeeey"                       -- "Liiiii}"  
rsDtCh23 = encode 1 "abcd"                          -- "bcde"  
rsDtCh24 = encode 5 "Marry Christmas! Ho ho ho!"    -- "Rfww~%Hmwnxyrfx&%Mt%mt%mt&"

--- decoding encoded msg ---
decode :: Int -> String -> String  
decode shift msg = encode (negate shift) msg

rsDtCh25 = encode 3 "Im a little teapot"                -- "Lp#d#olwwoh#whdsrw"  
rsDtCh26 = decode 3 "Lp#d#olwwoh#whdsrw"                -- "Im a little teapot"  
rsDtCh27 = decode 5 . encode 5 $ "This is a sentence"   -- "This is a sentence"


-- ============================== import Data.Map ========================

phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ] 

--- findKey ---    
findKey'' :: (Eq k) => k -> [(k,v)] -> v  
findKey'' key xs = snd . head . filter (\(k,v) -> key == k) $ xs
--
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey' key [] = Nothing  
findKey' key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKey' key xs
--    Note: It's usually better to use folds for this standard list recursion pattern instead of 
--    explicitly writing the recursion because they're easier to read and identify. Everyone knows 
--    it's a fold when they see the foldr call, but it takes some more thinking to read explicit recursion.                            
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing 
--
rsDtMp1 = findKey "penny" phoneBook                     -- Just "853-2492"  
rsDtMp2 = findKey "betty" phoneBook                     -- Just "555-2938"  
rsDtMp3 = findKey "wilma" phoneBook                     -- Nothing  

--- fromList --- The fromList function takes an association list (in the form of a list) and returns 
--      a map with the same associations
rsDtMp4 = Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]  
                          -- fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]  
rsDtMp5 = Map.fromList [(1,2),(3,4),(3,2),(5,5)]          -- fromList [(1,2),(3,2),(5,5)]
-- Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v 

--- empty --- represents an empty map. It takes no arguments, it just returns an empty map.
rsDtMp6 = Map.empty                                       -- fromList []  

--- insert --- takes a key, a value and a map and returns a new map that's just like the old one, 
--      only with the key and value inserted.
rsDtMp7 = Map.empty                                       -- fromList []  
rsDtMp8 = Map.insert 3 100 Map.empty                      -- fromList [(3,100)]  
rsDtMp9 = Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))  
                                                          -- fromList [(3,100),(4,200),(5,600)]  
rsDtMp10 = Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty  
                                                          -- fromList [(3,100),(4,200),(5,600)]  
--      We can implement our own fromList' by using the empty map, insert and a fold
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v  
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty  
--      It's a pretty straightforward fold. We start of with an empty map and we fold it up 
--      from the right, inserting the key value pairs into the accumulator as we go along.

--- null --- checks if a map is empty
rsDtMp11 = Map.null Map.empty                     -- True  
rsDtMp12 = Map.null $ Map.fromList [(2,3),(5,5)]  -- False  

--- size --- reports the size of a map.
rsDtMp13 = Map.size Map.empty                     -- 0  
rsDtMp14 = Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]  -- 5  

--- singleton --- takes a key and a value and creates a map that has exactly one mapping.
rsDtMp15 = Map.singleton 3 9                      -- fromList [(3,9)]  
rsDtMp16 = Map.insert 5 9 $ Map.singleton 3 9     -- fromList [(3,9),(5,9)]

--- lookup --- works like the Data.List lookup, only it operates on maps. It returns Just something 
--      if it finds something for the key and Nothing if it doesn't.

--- member --- is a predicate takes a key and a map and reports whether the key is in the map or not.
rsDtMp17 = Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]   -- True  
rsDtMp18 = Map.member 3 $ Map.fromList [(2,5),(4,5)]         -- False  

--- map --- and --- filter--- work much like their list equivalents.
rsDtMp19 = Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]  -- fromList [(1,100),(2,400),(3,900)]  
rsDtMp20 = Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]  
                                                              -- fromList [(2,'A'),(4,'B')]  

--- toList --- is the inverse of --- fromList ---.
rsDtMp21 = Map.toList . Map.insert 9 2 $ Map.singleton 4 3    -- [(4,3),(9,2)]  
--- keys --- and --- elems --- return lists of keys and values respectively. 
--      keys is the equivalent of "map fst . Map.toList" and 
--      elems is the equivalent of "map snd . Map.toList"

--- fromListWith --- is a cool little function. It acts like fromList, only it doesn't discard 
--      duplicate keys but it uses a function supplied to it to decide what to do with them. 
---     Let's say that a girl can have several numbers and we have an association list set up like this.
phoneBook' =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]
--      Now if we just use fromList to put that into a map, we'll lose a few numbers! 
--      So here's what we'll do:  
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs  
rsDtMp22 = Map.lookup "patsy" $ phoneBookToMap phoneBook'  -- "827-9162, 943-2929, 493-2928"  
rsDtMp23 = Map.lookup "wendy" $ phoneBookToMap phoneBook'  -- "939-8282"  
rsDtMp24 = Map.lookup "betty" $ phoneBookToMap phoneBook'  -- "342-2492, 555-2938"  

--      If a duplicate key is found, the function we pass is used to combine the values of 
--      those keys into some other value. We could also first make all the values in the association 
--      list singleton lists and then we can use ++ to combine the numbers.
phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]  
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs  
rsDtMp25 = Map.lookup "patsy" $ phoneBookToMap' phoneBook' -- ["827-9162","943-2929","493-2928"]  

--- Map.formListWith --- 
--      Another use case is if we're making a map from an association list of numbers and when 
--      a duplicate key is found, we want the biggest value for the key to be kept.
rsDtMp26 = Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]  
                                                            -- fromList [(2,100),(3,29),(4,22)]
rsDtMp27 = Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]  
                                                            -- fromList [(2,108),(3,62),(4,37)]

--- insertWith --- is to insert what fromListWith is to fromList. It inserts a key-value pair 
--      into a map, but if that map already contains the key, it uses the function passed to it 
--      to determine what to do.
rsDtMp28 = Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]  
                                                            -- fromList [(3,104),(5,103),(6,339)] 

-- ============================== import Data.Set ========================

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!" 

--- fromList --- The fromList function works much like you would expect. It takes a list and 
--      converts it into a set.
rsDtSt1 = Set.fromList text1                                -- fromList " .?AIRadefhijlmnorstuy"  
rsDtSt2 = Set.fromList text2                                -- fromList " !Tabcdefghilmnorstuvwy"
  
--- intersection --- function to see which elements they both share.  
rsDtSt3 = Set.intersection rsDtSt1 rsDtSt2                  -- fromList " adefhilmnorstuy"

--- difference --- to see which letters are in the first set but aren't in the second one and vice versa
rsDtSt4 = Set.difference rsDtSt1 rsDtSt2                    -- fromList ".?AIRj"  
rsDtSt5 = Set.difference rsDtSt2 rsDtSt1                    -- fromList "!Tbcgvw"

--- union --- we can see all the unique letters used in both sentences by using union
rsDtSt6 = Set.union rsDtSt1 rsDtSt2                         -- fromList " !.?AIRTabcdefghijlmnorstuvwy"

--- null --- size --- member --- empty --- singleton --- insert --- delete --- 
--      The null, size, member, empty, singleton, insert and delete functions all work like you'd expect.
rsDtSt7 = Set.null Set.empty                                -- True  
rsDtSt8 = Set.null $ Set.fromList [3,4,5,5,4,3]             -- False  
rsDtSt9 = Set.size $ Set.fromList [3,4,5,3,4,5]             -- 3  
rsDtSt10 = Set.singleton 9                                  -- fromList [9]  
rsDtSt11 = Set.insert 4 $ Set.fromList [9,3,8,1]            -- fromList [1,3,4,8,9]  
rsDtSt12 = Set.insert 8 $ Set.fromList [5..10]              -- fromList [5,6,7,8,9,10]  
rsDtSt13 = Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]      -- fromList [3,5]  

--- isSubsetOf --- isProperSubsetOf --- Set A is a subset of set B if B contains all the elements 
--      that A does. Set A is a proper subset of set B if B contains all the elements that A does 
--      but has more elements
rsDtSt14 = Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]             -- True  
rsDtSt15 = Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]         -- True  
rsDtSt16 = Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]   -- False  
rsDtSt17 = Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]           -- False  

--- toList --- Sets are often used to weed a list of duplicates from a list by first making it 
--      into a set with fromList and then converting it back to a list with toList. 
--      The Data.List function nub already does that, but weeding out duplicates for large lists is 
--      much faster if you cram them into a set and then convert them back to a list than using nub. 
--      But using nub only requires the type of the list's elements to be part of the Eq typeclass, 
--      whereas if you want to cram elements into a set, the type of the list has to be in Ord. 
setNub xs = Set.toList $ Set.fromList xs  
rsDtSt19 = setNub "HEY WHATS CRACKALACKIN"              -- " ACEHIKLNRSTWY"  
rsDtSt20 = nub "HEY WHATS CRACKALACKIN"                 -- "HEY WATSCRKLIN"
--      setNub is generally faster than nub on big lists but as you can see, 
--      nub preserves the ordering of the list's elements, while setNub does not

-- ============================== Making your own modules =====================================
-- see file Geometry.hs -- Version #1.
-- see Directory Geometry and files: Cube.hs, Sphere.hs, Cuboid.hs -- Version #2.

rsDtMd1 = Sphere.volume 10                  -- 4188.7905
rsDtMd2 = Cube.area 10                      -- 600.0
rsDtMd3 = Cuboid.volume 5.1 6.2 7.3         -- 230.826

rsDtMd4 = Geom.sphereVolume 10              -- 4188.7905
rsDtMd5 = Geom.cubeArea 10                  -- 600.0
rsDtMd6 = Geom.cuboidVolume 5.1 6.2 7.3     -- 230.826

-- ============================== Making your own types and Typeclasses =======================
---
data Shape' = Circle' Float Float Float | Rectangle' Float Float Float Float deriving (Show)
---
surface' :: Shape' -> Float  
surface' (Circle' _ _ r) = pi * r ^ 2  
surface' (Rectangle' x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
--
rsMyDt1 = surface' $ Circle' 10 20 10         -- 314.15927  
rsMyDt2 = surface' $ Rectangle' 0 0 100 100   -- 10000.0
--
rsMyDt3 = map (Circle' 10 20) [4,5,6,6]      
          -- [Circle' 10.0 20.0 4.0,Circle' 10.0 20.0 5.0,Circle' 10.0 20.0 6.0,Circle' 10.0 20.0 6.0]
---
rsMyDt4 = surface (Rectangle (Point 0 0) (Point 100 100))   -- 10000.0  
rsMyDt5 = surface (Circle (Point 0 0) 24)                   -- 1809.5574
rsMyDt6 = nudge (Circle (Point 34 34) 10) 5 10
rsMyDt7 = nudge (baseRect 40 100) 60 23         -- Rectangle (Point 60.0 23.0) (Point 100.0 123.0)  
------------------
rsMyDt8 = rsMyDtS4 -- rsMyDtS4 is imported from module Shapes.hs
--rsMyDt9 = rsMyDtS1 -- this one does not compile, because rsMyDtS1 is not exported

----------------------------------------------------
data Person' = Person' String String Int Float String String deriving (Show)
guy = Person' "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"  
        -- Person' "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
firstName' :: Person' -> String  
firstName' (Person' firstname' _ _ _ _ _) = firstname'  
--  
lastName' :: Person' -> String  
lastName' (Person' _ lastname' _ _ _ _) = lastname'  
--  
age' :: Person' -> Int  
age' (Person' _ _ age' _ _ _) = age'  
--  
height' :: Person' -> Float  
height' (Person' _ _ _ height' _ _) = height'  
--  
phoneNumber' :: Person' -> String  
phoneNumber' (Person' _ _ _ _ number' _) = number'  
--  
flavor' :: Person' -> String  
flavor' (Person' _ _ _ _ _ flavor') = flavor'
--
rsMyDt10 = firstName' guy      -- "Buddy"

-------- better way to write data type -----------------
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show, Ord, Eq, Read)

neo1  = Person "Neo" "Anderson" 40 184 "555-555" "butter-scotch"
neo2  = Person "Neo" "Anderson" 40 184 "555-555" "vanilla"
trin1 = Person "Trinity" "Unknown" 40 172 "555-666" "vanilla"
morf1 = Person {firstName     = "Morfeus"
                , lastName    = "Unknown0"
                , age         = 55  
                , height      = 182  
                , phoneNumber = "555-000"
                , flavor      = "chocolate"
                }
rsMyDt11 = neo1  == morf1             -- False
rsMyDt12 = neo1  >  trin1             -- False
rsMyDt13 = trin1 >  morf1             -- True
rsMyDt14 = neo2  >  neo1              -- True

---------- type parameters -----------------------------
--- data Maybe a = Nothing | Just a 
rsMyDt15 = Just "Haha"                -- Just "Haha"  
rsMyDt16 = Just 84                    -- Just 84  
--ghci> :t Just "Haha"  
                                      -- Just "Haha" :: Maybe [Char]  
--ghci> :t Just 84  
                                      -- Just 84 :: (Num t) => Maybe t  
--ghci> :t Nothing  
                                      -- Nothing :: Maybe a  
rsMyDt17 = Just 10 :: Maybe Double    -- Just 10.0  

------------------------
--data Car = Car  { company :: String
--                , model :: String 
--                , year :: Int               
--                } deriving (Show, Read, Eq)
--c1   = Car "lexus"  "RX350"  2014
--c2   = Car "lexus"  "RX350"  2014
--bDif = c1 == c2
rsMyDt18 = Car {company = "ford", model = "Mustang", year = 1967}
rsMyDt19 = c1 == rsMyDt18
rsMyDt20 = rsMyDt18 < c1 
---
--tellCar :: (Show a) => Car String String a -> String  
tellCar :: Car -> [Char]
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-------------------------
-- If we were defining a mapping type, we could add a typeclass constraint 
--      in the data declaration:
--      data (Ord k) => Map k v = ...  
-- !!! However, it's a very strong convention in Haskell to never add typeclass constraints 
--      in data declarations. !!! ------------
-- !!! Don't put type constraints into data declarations !!! --

--- data 3D Vector ---
data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

--- !!!  it's very important to distinguish between 
--       the type constructor and the value constructor( coulbe separeated by '|')
--           [--------------]     [---------------------------]
--             data Vector a   =   Vector a a a deriving (Show)

rsMyDt21 = Vector 3 5 8 `vplus` Vector 9 2 8                        -- Vector 12 7 16  
rsMyDt22 = Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3   -- Vector 12 9 19  
rsMyDt23 = Vector 3 9 7 `vectMult` 10                               -- Vector 30 90 70  
rsMyDt24 = Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0             -- 74.0  
rsMyDt25 = Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)  
                                                                    -- Vector 148 666 222

------------------ Basic Typeclasses --------------------------
-- A typeclass is a sort of interface that defines some behavior. If a type is a part of a 
--      typeclass, that means that it supports and implements the behavior the typeclass 
--      describes. A lot of people coming from OOP get confused by typeclasses because they 
--      think they are like classes in object oriented languages. 
--      Well, they're not. You can think of them kind of as Java interfaces, only better.

--- Eq --- is used for types that support equality testing. The functions: "==" and "/="
--- Ord --- is used for types that have an ordering. The functions: ">","<",">=","<=",compare 
--      Ordering is a type that can be GT, LT or EQ
--- Show --- Members of Show can be presented as strings. Function "show" is member of this typeclass
--- Read --- The read function takes a string and returns a type which is a member of Read. Func "read"
--- Enum --- Enum members are sequentially ordered types — they can be enumerated. The main 
--      advantage of the Enum typeclass is that we can use its types in list ranges. They also 
--      have defined successors and predecesors, which you can get with the "succ" and "pred" 
--      functions. Types in this class: (), Bool, Char, Ordering, Int, Integer, Float and Double.
--- Bounded --- Bounded members have an upper and a lower bound. "minBound", "maxBound", Int, Char, Bool
--- Num --- Num is a numeric typeclass. Its members have the property of being able to act 
--      like numbers. Let's examine the type of a number. Int, Integer, Float, Double
--- Integral --- Integral is also a numeric typeclass. Num includes all numbers, including 
--      real numbers and integral numbers, Integral includes only integral (whole) numbers. 
--      In this typeclass are Int and Integer
--- Floating --- Floating includes only floating point numbers, so Float and Double.

-- A very useful function for dealing with numbers is fromIntegral
--- fromIntegral ---
--      It has a type declaration of fromIntegral :: (Num b, Integral a) => a -> b. 
--      From its type signature we see that it takes an integral number and turns it into a 
--      more general number. That's useful when you want integral and floating point types 
--      to work together nicely. For instance, the length function has a type declaration of 
--      length :: [a] -> Int instead of having a more general type of (Num b) => length :: [a] -> b. 
--      if we try to get a length of a list and then add it to 3.2, we'll get an error because 
--      we tried to add together an Int and a floating point number. So to get around this, 
--      we do fromIntegral (length [1,2,3,4]) + 3.2 and it all works out.



------------------ Derived instances -----------------------------------------
-- Example: the Int type is an instance of the Eq typeclass because the Eq typeclass defines 
--      behavior for stuff that can be equated.
---------------- typeclasses: Eq, Ord, Enum, Bounded, Show, Read -------------
{-
neo1  = Person "Neo" "Anderson" 40 184 "555-555" "butter-scotch"
neo2  = Person "Neo" "Anderson" 40 184 "555-555" "vanilla"
trin1 = Person "Trinity" "Unknown" 40 172 "555-666" "vanilla"
morf1 = Person {firstName     = "Morfeus"
                , lastName    = "Unknown0"
                , age         = 55  
                , height      = 182  
                , phoneNumber = "555-000"
                , flavor      = "chocolate"
                }
-}
rsMyDt26 = read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43, height = 178, phoneNumber = \"555-999\", flavor = \"vanilla\" }" :: Person
      -- Person {firstName = "Michael", lastName = "Diamond", age = 43, height = 178.0, phoneNumber = "555-999", flavor = "vanilla"}
rsMyDt27 = read "Person {firstName =\"Neo\", lastName =\"Anderson\", age = 40, height = 184, phoneNumber = \"555-555\", flavor = \"butter-scotch\" }" :: Person  
      -- Person {firstName = "Neo", lastName = "Anderson", age = 40, height = 184.0, phoneNumber = "555-555", flavor = "butter-scotch"}
rsMyDt28 = rsMyDt27 == neo1

--- !!!! We can also read parameterized types, but we have to fill in the type parameters. 
--      So we can't do read "Just 't'" :: Maybe a, but we can do read "Just 't'" :: Maybe Char.
rsMyDt29 = "Just 80"
rsMyDt32 = "Just 't'"
rsMyDt33 = "Nothing"
rsMyDt30 = read rsMyDt29 :: Maybe Int          -- Just 80
rsMyDt31 = read rsMyDt32 :: Maybe Char         -- Just 't'
rsMyDt34 = read rsMyDt33 :: Maybe Char         -- Nothing
rsMyDt35 = read rsMyDt33 :: Maybe Int          -- Nothing 
------------------------------ 
{-
λ>:i Maybe
data Maybe a = Nothing | Just a   -- Defined in ‘GHC.Maybe’
instance Applicative Maybe        -- Defined in ‘GHC.Base’
instance Eq a => Eq (Maybe a)     -- Defined in ‘GHC.Maybe’
instance Functor Maybe            -- Defined in ‘GHC.Base’
instance Monad Maybe              -- Defined in ‘GHC.Base’
instance MonadPlus Maybe          -- Defined in ‘GHC.Base’
instance Semigroup a => Monoid (Maybe a)      -- Defined in ‘GHC.Base’
instance Ord a => Ord (Maybe a)   -- Defined in ‘GHC.Maybe’
instance Semigroup a => Semigroup (Maybe a)   -- Defined in ‘GHC.Base’
instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
instance Read a => Read (Maybe a) -- Defined in ‘GHC.Read’
instance Foldable Maybe           -- Defined in ‘Data.Foldable’
instance Traversable Maybe        -- Defined in ‘Data.Traversable’
instance MonadFail Maybe          -- Defined in ‘Control.Monad.Fail’
-}
---------
-- data Bool = False | True deriving (Ord) 
rsMyDt36 = True `compare` False         -- GT  
rsMyDt37 = True > False                 -- True  
--
rsMyDt38 = Nothing < Just 100           -- True  
rsMyDt39 = Nothing > Just (-49999)      -- False  
rsMyDt40 = Just 3 `compare` Just 2      -- GT  
rsMyDt41 = Just 100 > Just 50           -- True  

--- But we can't do something like Just (*3) > Just (*2), because (*3) and (*2) are functions,
--      which aren't instances of Ord

--------------------------------
-- data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday  
--      Because all the value constructors are nullary (take no parameters, i.e. fields), 
--      we can make it part of the Enum typeclass. The Enum typeclass is for things that 
--      have predecessors and successors. We can also make it part of the Bounded typeclass,
--      which is for things that have a lowest possible value and highest possible value. 
--      And while we're at it, let's also make it an instance of all the other derivable 
--      typeclasses and see what we can do with it.

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  
--      Because it's part of the Show and Read typeclasses, we can convert values of this type
--      to and from strings.

rsMyDt42 = Wednesday                    -- Wednesday  
rsMyDt43 = show Wednesday               -- "Wednesday"  
rsMyDt44 = read "Saturday" :: Day       -- Saturday  

--      Because it's part of the Eq and Ord typeclasses, we can compare or equate days.
rsMyDt45 = Saturday == Sunday           -- False  
rsMyDt46 = Saturday == Saturday         -- True  
rsMyDt47 = Saturday > Friday            -- True  
rsMyDt48 = Monday `compare` Wednesday   -- LT  

--      It's also part of Bounded, so we can get the lowest and highest day.
rsMyDt49 = minBound :: Day              -- Monday  
rsMyDt50= maxBound :: Day               -- Sunday  

--      It's also an instance of Enum. We can get predecessors and successors of days 
--      and we can make list ranges from them!
rsMyDt51 = succ Monday                  -- Tuesday  
rsMyDt52 = pred Saturday                -- Friday  
rsMyDt53 = [Thursday .. Sunday]         -- [Thursday,Friday,Saturday,Sunday]  
rsMyDt54 = [minBound .. maxBound] :: [Day]  -- [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday] 

-- N.B. succ Sunday *** Exception: succ{Day}: tried to take `succ' of last tag in enumeration
--      pred Monday *** Exception: pred{Day}: tried to take `pred' of first tag in enumeration

------------------------- type synonyms --------------------------
-- type String = [Char]

--      Let's make a type synonym to convey some more information in the type declaration
--type PhoneBook   = [(String,String)]
--      Now the type declaration for our phonebook can be phoneBook :: PhoneBook. Let's 
--      make a type synonym for String as well.
type PhoneNumber  = String  
type Name         = String  
type PhoneBook    = [(Name,PhoneNumber)]

-- Type synonyms can also be parameterized. If we want a type that represents an association 
--      list type but still want it to be general so it can use any type as the keys and 
--      values, we can do this:
type AssocList k v = [(k,v)] 
--      Now, a function that gets the value by a key in an association list can have a type of 
--      (Eq k) => k -> AssocList k v -> Maybe v. AssocList is a type constructor that takes two 
--      types and produces a concrete type, like AssocList Int String, for instance.

--- Just like we can partially apply functions to get new functions, we can partially apply 
--      type parameters and get new type constructors from them. Just like we call a function 
--      with too few parameters to get back a new function, we can specify a type constructor 
--      with too few type parameters and get back a partially applied type constructor. If we 
--      wanted a type that represents a map (from Data.Map) from integers to something, we could
--      either do this:
type IntMap1 v = Map.Map Int v  
--      Or we could do it like this:
type IntMap2 = Map.Map Int  
--      Either way, the IntMap type constructor takes one parameter and that is the type of 
--      what the integers will point to
--      If you're going to try and implement this, you'll probably going to do a qualified 
--      import of Data.Map. When you do a qualified import, type constructors also have to be 
--      preceeded with a module name. So you'd write type IntMap = Map.Map Int.
------------------------------------
--      Make sure that you really understand the distinction between type constructors and 
--      value constructors. Just because we made a type synonym called IntMap or AssocList 
--      doesn't mean that we can do stuff like AssocList [(1,2),(4,5),(7,9)]. All it means is 
--      that we can refer to its type by using different names. 
--      We can do [(1,2),(3,5),(8,9)] :: AssocList Int Int, which will make the numbers inside 
--      assume a type of Int, but we can still use that list as we would any normal list that 
--      has pairs of integers inside. Type synonyms (and types generally) can only be used in 
--      the type portion of Haskell. We're in Haskell's type portion whenever we're defining 
--      new types (so in data and type declarations) or when we're located after 
--      a ::. The :: is in type declarations or in type annotations.

--- Either --- Another cool data type that takes two types as its parameters is the 
--      Either a b type. This is roughly how it's defined:
--data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show) 
--      It has two value constructors. If the Left is used, then its contents are of type a and
--      if Right is used, then its contents are of type b. So we can use this type to 
--      encapsulate a value of one type or another and then when we get a value of type 
--      Either a b, we usually pattern match on both Left and Right and we different stuff 
--      based on which one of them it was.
rsMyDt55 = Right 20           -- Right 20  
rsMyDt56 = Left "w00t"        -- Left "w00t"  

rsMyDt57 = Right 'a' :: Either a Char  
rsMyDt58 = Left True :: Either Bool b  

rsMyDt58' = Left "Something" :: Either String a  

-- another implementation of head, using Either, safe one
-- Listing 38.6. A safer version of head written using Either
-- https://livebook.manning.com/book/get-programming-with-haskell/chapter-38/90

eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x:xs) = Right x

rsMyDt64 = eitherHead [1,2,3,4,5]
rsMyDt65 = eitherHead "This is a test string"


----------- locker example -----------
data LockerState = Taken | Free deriving (Show, Eq)    
type Code = String    
type LockerMap = Map.Map Int (LockerState, Code)
---
lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
--
lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  
-- Now let's try looking up some locker codes.
rsMyDt59 = lockerLookup 101 lockers       -- Right "JAH3I"  
rsMyDt60 = lockerLookup 100 lockers       -- Left "Locker 100 is already taken!"  
rsMyDt61 = lockerLookup 102 lockers       -- Left "Locker number 102 doesn't exist!"  
rsMyDt62 = lockerLookup 110 lockers       -- Left "Locker 110 is already taken!"  
rsMyDt63 = lockerLookup 105 lockers       -- Right "QOTSA"

----------- recursive data structure ------------------
rsMyDt66 = 3:(4:(5:6:[])) 
rsMyDt67 = 3:4:5:6:[] 
rsMyDt68 = [3,4,5,6]

--- our own List ---
data List' a = Empty' | Cons' a (List' a) deriving (Show, Read, Eq, Ord)
data List'' a = Empty'' | Cons'' { listHead :: a, listTail :: List'' a} deriving (Show, Read, Eq, Ord)
---
rsMyDt69 = Empty''                                  -- Empty''  
rsMyDt70 = 5 `Cons''` Empty''                         -- Cons'' 5 Empty''  
rsMyDt71 = 4 `Cons''` (5 `Cons''` Empty'')              -- Cons'' 4 (Cons'' 5 Empty'')  
rsMyDt72 = 3 `Cons''` (4 `Cons''` (5 `Cons''` Empty''))   -- Cons'' 3 (Cons'' 4 (Cons'' 5 Empty''))

----------- intermission fixity declaration -----------------
--      We can define functions to be automatically infix by making them comprised of only 
--      special characters. We can also do the same with constructors, since they're just 
--      functions that return a data type
--      fixity for ^         infixr 8  
--      fixity for *         infixl 7  
--      fixity for +         infixl 6  
--      fixity for -         infixl 6  
--      fixity for :         infixr 5
--      fixity for ==        infix  4

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
--      now we can rewrite our list like this
rsMyDt73 = 3 :-: 4 :-: 5 :-: Empty                -- (:-:) 3 ((:-:) 4 ((:-:) 5 Empty))  
rsMyDt74 = 3 :-: 4 :-: 5 :-: Empty  
rsMyDt75 = 100 :-: rsMyDt74                       -- (:-:) 100 ((:-:) 3 ((:-:) 4 ((:-:) 5 Empty))) 

---- this is how ++ defined
-- infixr 5  ++ 
-- (++) :: [a] -> [a] -> [a]  
-- []     ++ ys = ys  
-- (x:xs) ++ ys = x : (xs ++ ys)

--- make our own .++
infixr 5  .++  
(.++) :: List a -> List a -> List a   
Empty .++ ys = ys                         
(x :-: xs) .++ ys = x :-: (xs .++ ys)

---
rsMyDt76 = 3 :-: 4 :-: 5 :-: Empty  
rsMyDt77 = 6 :-: 7 :-: Empty  
rsMyDt78 = rsMyDt76 .++ rsMyDt77          -- (:-:) 3 ((:-:) 4 ((:-:) 5 ((:-:) 6 ((:-:) 7 Empty))))

---------------- binary search tree ----------------------
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- utility func for making singltone tree (just one node)
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  

-- func to insert an element into a tree  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)

-- func to check if some element is in the tree?
treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right

-- we'll use a fold to build up a tree from a list.
nums = [8,6,4,1,7,3,5]  
numsTree = foldr treeInsert EmptyTree nums  
--numsTree  -- Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

rsMyDt79 = 8 `treeElem` numsTree      -- True  
rsMyDt80 = 100 `treeElem` numsTree    -- False  
rsMyDt81 = 1 `treeElem` numsTree      -- True  
rsMyDt82 = 10 `treeElem` numsTree     -- False  

numsTree1 :: Tree Integer
numsTree1 = foldr treeInsert EmptyTree [3,99, (-1), 10, 11, 6, 8, 77, 100, (-5)]

--- ============================== typeclasses again ===========================
-- typeclasses have nothing to do with "class" from imperative language, like C, 
-- Java, Python !!!
--
-- this is how class Eq is defined in prelude:
----------------------------------
--class Eq a where  
--    (==) :: a -> a -> Bool  
--    (/=) :: a -> a -> Bool  
--    x == y = not (x /= y)  
--    x /= y = not (x == y)
----------------------------------

---- TrafficLight example creating our own typeclass and us it----
data TrafficLight = Red | Yellow | Green

---     here we are making an instance of Eq
instance Eq TrafficLight where  
    Red    == Red    = True  
    Green  == Green  = True  
    Yellow == Yellow = True  
    _      == _      = False

--- 
instance Show TrafficLight where  
    show Red    = "Red light"  
    show Yellow = "Yellow light"  
    show Green  = "Green light"  
{-
-- :t does not work, because of no contructor yet (Data constructor not in scope: TrafficLight)
λ> :i TrafficLight
data TrafficLight = Red | Yellow | Green
        -- Defined at /Users/admin1/Haskell/PROJECTS/L4/src/Lib4.hs:3748:1
instance [safe] Eq TrafficLight
  -- Defined at /Users/admin1/Haskell/PROJECTS/L4/src/Lib4.hs:3751:10
instance [safe] Show TrafficLight
  -- Defined at /Users/admin1/Haskell/PROJECTS/L4/src/Lib4.hs:3758:10
-}

--      Once again, we used pattern matching to achieve our goals. Let's see how it works 
--      in action:
rsMyOTC1 = Red == Red                       -- True  
rsMyOTC2 = Red == Yellow                    -- False  
rsMyOTC3 = Red `elem` [Red, Yellow, Green]  -- True  
rsMyOTC4 = [Red, Yellow, Green]             -- [Red light,Yellow light,Green light]  

---     But how are the Maybe or list types made as instances of typeclasses? 
--      What makes Maybe different from, say, TrafficLight is that Maybe in itself isn't 
--      a concrete type, it's a type constructor that takes one type parameter 
--      (like Char or something) to produce a concrete type (like Maybe Char).
---------------------------------
-- this is how we could write:
--instance Eq (Maybe m) where  
--    Just x  == Just y  = x == y  
--    Nothing == Nothing = True  
--    _       == _       = False 
---------------------------------
-- There's one problem with this though. Can you spot it? We use == on the contents 
--      of the Maybe but we have no assurance that what the Maybe contains can be used with Eq! That's why we have to modify our instance declaration like this:
---------------------------------
--instance (Eq m) => Eq (Maybe m) where  
--    Just x  == Just y  = x == y  
--    Nothing == Nothing = True  
--    _       == _       = False  
---------------------------------        
--    Take into account that the type you're trying to make an instance of will replace 
--    the parameter in the class declaration. The a from class Eq a where will be replaced 
--    with a real type when you make an instance, so try mentally putting your type into 
--    the function type declarations as well. 
--    "(==) :: Maybe -> Maybe -> Bool" doesn't make much sense but 
--    "(==) :: (Eq m) => Maybe m -> Maybe m -> Bool" does. 
--    But this is just something to think about, because == will always have a type of 
--    (==) :: (Eq a) => a -> a -> Bool, no matter what instances we make.

--- YesNo Typeclass Example ---
class YesNo a where  
    yesno :: a -> Bool
---------------
instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True 
--
instance YesNo [a] where  
    yesno [] = False  
    yesno _  = True  
--
instance YesNo Bool where  
    yesno = id
-- What's id? It's just a standard library function that takes a parameter and returns 
--    the same thing, which is what we would be writing here anyway.    
--    id True  = True
--    id "abc" = "abc"
--    id Nothing = Nothing
--
instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False
--
instance YesNo (Tree a) where  
    yesno EmptyTree = False  
    yesno _ = True
-- 
instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _   = True 

---
rsMOTC6 = yesno $ length []         -- False  
rsMOTC7 = yesno ("haha" :: [Char])  -- True  
rsMOTC8 = yesno ("" :: [Char])      -- False  
rsMOTC9 = yesno $ Just 0            -- True  
rsMOTC10 = yesno True               -- True  
rsMOTC11 = yesno EmptyTree          -- False  
rsMOTC12 = yesno []                 -- False  
rsMOTC13 = yesno [0,0,0]            -- True  
rsMOTC14 = yesno Green              -- True  
-- λ> :t yesno  
-- λ> yesno :: (YesNo a) => a -> Bool

--- function yesnoIf --- it mimics if statement
yesnoIf :: (YesNo y) => y -> a -> a -> a  
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult
---
rsMOTC15 = yesnoIf [] "YEAH!" "NO!"         -- "NO!"  
rsMOTC16 = yesnoIf [2,3,4] "YEAH!" "NO!"    -- "YEAH!"  
rsMOTC17 = yesnoIf True "YEAH!" "NO!"       -- "YEAH!"  
rsMOTC18 = yesnoIf (Just 500) "YEAH!" "NO!" -- "YEAH!"  
rsMOTC19 = yesnoIf Nothing "YEAH!" "NO!"    -- "NO!"  

-- ============================ The Functor typeclass ==========================


-- ============================ Input and Output  ==========================
--- a very small program Main.hs
-- main = putStrLn "hello, world"    
-- $ ghc --make helloworld  
-- [1 of 1] Compiling Main             ( helloworld.hs, helloworld.o )  
-- Linking helloworld ...  
-- $ ./helloworld  
-- hello, world  
---------------------
-- > :t putStrLn  
-- putStrLn :: String -> IO ()  
-- > :t putStrLn "hello, world"  
-- putStrLn "hello, world" :: IO ()

--      Printing a string to the terminal doesn't really have any kind of meaningful return value, 
--      so a dummy value of () is used.

--   --------------------------------------------------------------------------
--  !!!   The empty tuple is a value of () and it also has a type of ().    !!!
--   --------------------------------------------------------------------------

---------- main :: IO something, where something is some concrete type
--  main = do  
--      putStrLn "Hello, what's your name?"  
--      name <- getLine  
--      putStrLn ("Hey " ++ name ++ ", you rock!") 
----------
-- > :t getLine  
-- getLine :: IO String 

---
func1main = do  
    foo <- putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")

-------------
--      Beginners sometimes think that doing
--
--      name = getLine  
--
--      will read from the input and then bind the value of that to name. Well, it won't, 
--      all this does is give the getLine I/O action a different name called, well, name. 
--      Remember, to get the value out of an I/O action, you have to perform it inside 
--      another I/O action by binding it to a name with <-.
-----
func2main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

----- this one will stop if an input is a empty line 
func3main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            func3main  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words

-----
--      Using return doesn't cause the I/O do block to end in execution or anything like that. 
--      For instance, this program will quite happily carry out all the way to the last line:
func4main = do  
    return ()                   -- does not do anything!, because no binding to any name!
    return "HAHAHA"             -- does not do anything!, because no binding to any name!
    line <- getLine  
    return "BLAH BLAH BLAH"     -- does not do anything!, because no binding to any name!
    return 4                    -- does not do anything!, because no binding to any name!
    putStrLn line  
--      All these returns do is that they make I/O actions that don't really do anything 
--      except have an encapsulated result and that result is thrown away because it isn't 
--      bound to a name. We can use return in combination with <- to bind stuff to names.

-----
func5main = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStr "test:\n"
    putStrLn $ a ++ " " ++ b
-----
--      So you see, return is sort of the opposite to <-. While return takes a value and 
--      wraps it up in a box, <- takes a box (and performs it) and takes the value out of it, 
--      binding it to a name. But doing this is kind of redundant, especially since you can 
--      use let bindings in do blocks to bind to names, like so:
func5'main = do  
    let a = "hell"  
        b = "yeah"  
    putStr "test:\n"
    putStrLn $ a ++ " " ++ b


-- ============== some IO functions =================================================== 
-- putStrLn, putStr, putChar, print, getChar, when, sequence, mapM, forever, forM 

--      putStr is actually defined recursively with the help of putChar. The edge condition of 
--      putStr is the empty string, so if we're printing an empty string, just return an I/O 
--      action that does nothing by using return (). If it's not empty, then print the first 
--      character of the string by doing putChar and then print of them using putStr

----- this is how putStr was acctually defined
putStr' :: String -> IO ()  
putStr' [] = return ()              -- recursion in IO
putStr' (x:xs) = do  
    putChar x  
    putStr xs

--- print 
--      print takes a value of any type that's an instance of Show (meaning that we know how 
--      to represent it as a string), calls show with that value to stringify it and then 
--      outputs that string to the terminal. Basically, it's just (putStrLn . show). It first 
--      runs show on a value and then feeds that to putStrLn, which returns an I/O action that 
--      will print out our value.

--- getChar
--      getChar is an I/O action that reads a character from the input. Thus, its type 
--      signature is getChar :: IO Char, because the result contained within the I/O action 
--      is a Char. Note that due to buffering, reading of the characters won't actually happen 
--      until the user mashes the return key.
func6main = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            func6main  
        else return ()    

--- when 
--      The when function is found in Control.Monad (to get access to it, do import 
--      Control.Monad). It's interesting because in a do block it looks like a control flow 
--      statement, but it's actually a normal function. It takes a boolean value and an 
--      I/O action if that boolean value is True, it returns the same I/O action that we 
--      supplied to it. However, if it's False, it returns the return (), action, so an I/O 
--      action that doesn't do anything. Here's how we could rewrite the previous piece of code 
--      with which we demonstrated getChar by using when:
--import Control.Monad   
func7main = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        func7main  

--- sequence takes a list of I/O actions and returns an I/O actions that will perform those 
--      actions one after the other. The result contained in that I/O action will be a list of 
--      the results of all the I/O actions that were performed. Its type signature is 
----    sequence :: [IO a] -> IO [a]. Doing this:
func8main = do  
    a <- getLine  
    b <- getLine  
    c <- getLine  
    print [a,b,c]  
------
func9main = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs
---     A common pattern with sequence is when we map functions like print or putStrLn over lists. 
--      Doing map print [1,2,3,4] won't create an I/O action. It will create a list of 
--      I/O actions, because that's like writing [print 1, print 2, print 3, print 4]. If we 
--      want to transform that list of I/O actions into an I/O action, we have to sequence it.
rsIO1 = sequence (map print [1,2,3,4,5])

-----   Because mapping a function that returns an I/O action over a list and then sequencing 
--      it is so common, the utility functions mapM and mapM_ were introduced. mapM takes a 
--      function and a list, maps the function over the list and then sequences it. 
--      mapM_ does the same, only it throws away the result later. We usually use mapM_ when 
--      we don't care what result our sequenced I/O actions have
rsIO2 = mapM print [1,2,3]          -- 1  
                                    -- 2  
                                    -- 3  
rsIO3 = mapM_ print [1,2,3]         -- 1  
                                    -- 2                                   
                                    -- 3  
rsIO4 = mapM_ putChar ['a', 'b', 'c', '\n']        -- abc                            

---- forever takes an I/O action and returns an I/O action that just repeats the I/O action 
--      it got forever. It's located in Control.Monad. This little program will indefinitely 
--      ask the user for some input and spit it back to him, CAPSLOCKED:
--import Control.Monad  
--import Data.Char  
func10main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l  

---- forM (located in Control.Monad) is like mapM, only that it has its parameters switched 
--      around. The first parameter is the list and the second one is the function to map over 
--      that list, which is then sequenced. Why is that useful? Well, with some creative use of 
--      lambdas and do notation, we can do stuff like this:
--import Control.Monad  
func11main = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  
--      You can think of forM as meaning: make an I/O action for every element in this list. 
--      What each I/O action will do can depend on the element that was used to make the action.
--      Finally, perform those actions and bind their results to something. We don't have to 
--      bind it, we can also just throw it away.

-- ============================ Files and streams ==================================
--- getContents --- getChar is an I/O action that reads a single character from the terminal. 
--      getLine is an I/O action that reads a line from the terminal. These two are pretty 
--      straightforward and most programming languages have some functions or statements that 
--      are parallel to them. But now, let's meet getContents. getContents is an I/O action 
--      that reads everything from the standard input until it encounters an end-of-file 
--      character. Its type is getContents :: IO String. What's cool about getContents is that 
--      it does lazy I/O. When we do foo <- getContents, it doesn't read all of the input 
--      at once, store it in memory and then bind it to foo. No, it's lazy! It'll say: "Yeah 
--      yeah, I'll read the input from the terminal later as we go along, when you really need it!".
--import Control.Monad  
--import Data.Char 
func12main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l
--- Haiku.txt
-- I'm a lil' teapot
-- What's with that airplane food, huh?
-- It's so small, tasteless    

-- I'm a lil' teapot\nWhat's with that airplane food, huh?\nIt's so small, tasteless

--- So what we're essentially doing with that use of forever is taking the input and transforming
--      it into some output. That's why we can use getContents to make our program even shorter 
--      and better:
-- import Data.Char    
func13main = do  
    contents <- getContents  
    putStr (map toUpper contents)
----
-- $ cat haiku.txt | ./capslocker  
-- I'M A LIL' TEAPOT  
-- WHAT'S WITH THAT AIRPLANE FOOD, HUH?  
-- IT'S SO SMALL, TASTELESS

-- Let's make program that takes some input and prints out only those lines that are shorter 
--      than 10 characters.
func14main = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
---  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result
-----
-- 12345678\n1234567890\n12\nqwertyuiop\n



















