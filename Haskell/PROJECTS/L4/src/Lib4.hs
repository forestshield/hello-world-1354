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
            (quicksort [(10,2,5),(3,1,6),(-99,4,2), (3,4.0,8)]), 
            (quicksort [10,2,5,3,1,6,-99,4,2,3,4.0,8,9]))
           "\nquicksort [10,2,4,4,8,9]\nquicksort \"the quick brown fox jumps over the lazy dog\"\n\
           \quicksort [(10,2,5),(3,1,6),(-99,4,2), (3,4.0,8)]\n\
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
           (enumFrom BB), (enumFrom Green) )
           ",\ntake 10 (enumFrom 'a')\ntake 10 (enumFrom 23)\n\
           \     data XXX = AA|BB|CC|DD deriving (Enum, Show)\n\
           \enumFrom BB\n\
           \     data Color  = Blue | Green | Read deriving (Show, Read, Eq, Enum)\n\
           \enumFrom Green"
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
            --(do b ++ a where (a,b) = splitAt 3 "foobar"), -- does not compile
            (rsDtL28'))
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
{-           
 specShow ((zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2] )
--           (zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]))
--           "\n(zipWith3 (\\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]\
--           \\nzip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]"
           "\n\n\
           \\n"
           "zipWith3,4,5,6,7 --- zip3,4,5,6,7"
-}           
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

-- some cool stuff
  putStrLn $ show $ sum' []
  putStrLn $ show $ length ("abcdef" :: String)           
{-
  specShow ((), (), 
            (), ())
           "\n\n\
           \\n"
           ""
  specShow ()
           "\n\
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
data Color  = Blue | Green | Read deriving (Show, Read, Eq, Enum) 
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
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted

-- quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9] -- [1,2,2,3,3,4,4,5,6,7,8,9,10]
-- quicksort "the quick brown fox jumps over the lazy dog" -- "        abcdeeefghhijklmnoooopqrrsttuuvwxyz"
-- quicksort [10,2,5,3,1,6,-99,4,2,3,4.0,8,9] -- [-99.0,1.0,2.0,2.0,3.0,3.0,4.0,4.0,5.0,6.0,8.0,9.0,10.0]
-- quicksort [(10,2,5),(3,1,6),(-99,4,2), (3,4.0,8)] -- [(-99,4.0,2),(3,1.0,6),(3,4.0,8),(10,2.0,5)]

-- curried functions ----------
resCur1 = max 4 5 
resCur2 = (max 4) 5

-- max :: (Ord a) => a -> a -> a

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

