-- N.B. !!! To print properly Unocode strings on Windows PowerShell and avoid console eception
-- "*** Exception: <stdout>: hPutChar: invalid argument (invalid character)"
-- type "chcp 65001" in powershell before using ghci !!!
-- use ConEmu on Windows, "https://www.fosshub.com/ConEmu.html"
-- It is also done programaticaly in main function


{-
module Lib 
    (someFunc
    ,strOS
    ,strArch
    ,addZero
    ,someFunc2
    ,someFunc3
    ,someFunc4
    ) where
-}
module Lib where

import Data.Tuple
import Data.Typeable
import GHC.Int
import System.Info 
import Data.String
import System.Directory 

--import Data.Foldable (foldMap)
--import Data.Text (pack, Text)

--someFunc
someFunc :: IO ()
someFunc = do
  putStrLn "N.B. !!! To print properly Unocode strings on Windows PowerShell and avoid console eception\
           \\n*** Exception: <stdout>: hPutChar: invalid argument (invalid character)\
           \\ntype \"chcp 65001\" in powershell before using ghci !!!\
           \\nuse ConEmu on Windows, \"https://www.fosshub.com/ConEmu.html\"\n"

  putStrLn "================================= Lib ==================================="
  funcCheckOS
  
  --- getHomeDirectory is not a function but an IO action so you have to unpack it 
  --- within another IO action first.
  getHomeDirectory >>= print     -- getHomeDirectory returns only "IO FilePath", not IO !!!

  let x = 2
      y = 2.0
      bVal = x == y           -- λλλ, 2 == 2.0
      bVal2 = y == z          -- True
      z = 2.1
      bVal3 = x < 3           -- True
      bVal4 = x > 5           -- False
      bVal5 = x /= z          -- True
  print bVal4    
  print bVal3
  print bVal5                     -- True
  putStrLn "λλλ, 2 == 2.0"
  
  -- hlint 
  --putStrLn $ show bVal          -- True
  print bVal                      -- True    
  putStrLn "λλλ, 2.0 == 2.1"      -- λλλ, 2.0 == 2.1
  --putStrLn $ show bVal2         -- False
  print bVal2                      -- False

  let bVal6 = 'a' < 'b'           -- True
      bVal7 = "Julie" == "Chris"  -- False
      bVal8 = "Julie" > "Chris"   -- True
      bVal9 = "Julia" < "Julie"   -- True
      bVal10 = "Yulia" < "Julie"  -- False

  putStr (show bVal6)    
  putStrLn " λλλ, 'a' < 'b'"           -- λλλ, 2.0 == 2.1    
  
  putStr (show bVal7)                  -- False
  putStrLn " λλλ, Julie == Chris"      -- λλλ, Julie == Chris    
  
  putStr (show bVal8)                  -- True
  putStrLn " λλλ, Julie > Chris"       -- λλλ, Julie > Chris    
  
  putStr (show bVal9)                  -- True
  putStrLn " λλλ, Julia < Julie"       -- λλλ, Julia > Julie    
  
  putStr (show bVal10)                 -- False
  putStrLn " λλλ, Yulia < Julie"       -- λλλ, Yulia > Julie    

  putStrLn "\n====================== System OS Stuff ============================"
  print $ setOS2Var os 


-- ====================== System OS Stuff ============================ --
---
data CurOS   = MacOSX | Linux | Windows | Unknown deriving (Show, Read, Eq, Enum)
--data CurArch =  

--setOS2Var 
setOS2Var :: (Eq a, IsString a) => a -> CurOS
setOS2Var x  
  | x == "darwin"  = MacOSX
  | x == "linux"   = Linux
  | x == "mingw32" = Windows
  | otherwise      = Unknown

--oScur :: CurOs
--oScur = setOS2Var os
---
rsStr01 = "/Haskell/PROJECTS/"
rsStr02 = "\\Haskell\\PROJECTS\\"

--rsFIO01 :: IO FilePath
rsFIO01 = getHomeDirectory

--rsStr03 :: String 
--rsStr03 = (getHomeDirectory :: String)
---
pathFileOne :: Data.String.IsString p => CurOS -> p
pathFileOne x   
  | x == MacOSX  = "/User/admin1/Haskell/PROJECTS/"
--  | x == MacOSX  = ((rsFIO01 :: String) ++ rsStr01)
  | x == Linux   = "/Home/admin1/Haskell/PROJECTS/"
  | x == Windows = "C:\\Haskell\\PROJECTS\\"
  | otherwise    = ""

-- import Data.Foldable (foldMap)
-- import Data.Text (pack, Text)
--toText :: [FilePath] -> Text
--toText = foldMap pack  
    
{-
  pathFile2 setOS2Var os 
    | setOS2Var == MacOSX  = "/User/admin1/Haskell/PROJECTS/"
    | setOS2Var == Linux   = "/Home/admin1/Haskell/PROJECTS/"
    | setOS2Var == Windows = "C:\\Haskell\\PROJECTS\\"
    | otherwise            = ""
-}

--rsFIO1 :: IO FilePath
rsFIO1 = getHomeDirectory 

{-  
  case x of
    "darwin"  -> MacOSX
    "linux"   -> Linux
    "mingw32" -> Windows
  --    Unknown
-}
{-
linux"
"x86_64"
"ghc"
Version {versionBranch = [8,8], versionTags = []}

"darwin"
"x86_64"
"ghc"
Version {versionBranch = [8,8], versionTags = []}

"mingw32"
"x86_64"
"ghc"
Version {versionBranch = [8,8], versionTags = []}
-}

-----
strOS   :: [Char]
strOS   = os 
--
strArch :: [Char]
strArch = arch 

--funcCheckOS
funcCheckOS :: IO ()
funcCheckOS = do 
  --putStrLn "---------------------- checking OS ------------------------"
  print os
  print arch
  print compilerName
  print compilerVersion
  
{-
--setOS2Var
setOS2Var :: String -> CurOs 
setOS2Var x = 
  case os of
    "darwin"  -> MacOSX
    "linux"   -> Linux
    "mingw32" -> Windows
-}     
{-      
setOS2Var x  
  | "darwin"   MacOSX
    "linux"   -> Linux
    "mingw32" -> Windows
-}
--thisOS :: CurOS


--someFunc2
someFunc2 :: IO ()
someFunc2 = do
  let bVal = ['a', 'b'] > ['b', 'a']
      bVal1 = [1, 2, 3] > [2, 1]        
      bNotVal = not bVal1               -- True
      bAndVal = True && True            -- True
      bOrVal = False && True            -- True
  putStr (show bVal)                    -- False
  --putStrLn " λλλ, ['a', 'b'] > ['b', 'a']" -- λλλ, ['a', 'b'] > ['b', 'a']    
  -- Windows shell or PowerShell creates an exception when Unicode symbol is used
  -- There is a work around, we will do it later on
  putStrLn "['a', 'b'] > ['b', 'a']" -- ['a', 'b'] > ['b', 'a']    
  putStr (show bVal1)                   -- False
  putStrLn "True, not bVal1"       -- True    
  putStrLn "True, True && True"    -- True
  putStrLn "True, False || True"   -- True

--someFunc3
someFunc3 :: IO ()
someFunc3 = do
  let bt = "Truthin"
      bf = "Falsein"
      sr = (if True then bt else bf)

  putStr ("bt = '" ++ sr ++ "'")
  putStrLn "if True then bt else bf"   -- Truthin

--someFunc4
someFunc4 :: IO ()
someFunc4 = putStrLn str where
  bt = "Truthin"
  bf = "Falsein"
  sr = if True then bt else bf
  --str = " λλλ " ++ sr ++ ", if True then bt else bf"   -- Truthin
  str = " Test " ++ sr ++ ", if True then bt else bf"   -- Truthin

--funcChekIfCool
funcChekIfCool :: String -> IO ()
funcChekIfCool str = 
  if checkV 
    then putStrLn "Cool" 
  else 
    putStrLn "Not Cool"            
  where checkV = str == "I am Cool" 

--Tuples
twoT1 :: (Int, Bool)
twoT1 = (,) 5 True

-- can have same type in both members
twoT2 :: (Bool, Bool)
twoT2 = (,) False True

threeT :: (Integer, Char, [Char])
threeT = (5, 'a', "abc")

mTulp1 :: (Integer, [Char])
mTulp1 = (8 :: Integer, "Julie")

mTulp2 :: ([Char], Integer)
mTulp2 = swap mTulp1

sT1 :: (Int, [Int])
sT1 = (3, [3])
sT2 :: (Int, [Int])
sT2 = (5, [5])

--funcTuples
funcTuples :: IO ()  
funcTuples = do
  let tsT2 = swap twoT2
      t1 = fst twoT2
      t2 = snd (swap twoT2)
  --'fst' and 'snd' do not work on tuples in size more than 2     
  let x = fst3 threeT 
      y = snd3 threeT
      z = trd3 threeT
  let a = fst mTulp1      
      b = fst mTulp2
      c = snd mTulp1
      d = snd mTulp2      
  let e = tupFunc sT1 sT2
      --f = sT1 + sT2

  putStrLn ("fst=" ++ show t1 ++ ", twoT2(False, True)")
  putStrLn ("snd=" ++ show t2 ++ ", swap twoT2 (False, True)")

  putStrLn ("fst3=" ++ show x ++ ", treeT(5, 'a', 'abc')")
  putStrLn ("snd3=" ++ show y ++ ", treeT(5, 'a', 'abc')")
  putStrLn ("trd3=" ++ show z ++ ", treeT(5, 'a', 'abc')")

  putStr ("fst=" ++ show a ++ ", mTulp1(8, 'Julie'), isInteger = ")
  print $ isInteger a

  putStr ("fst=" ++ show b ++ ", mTulp2('Julie', 8), isInteger = ")
  print $ isInteger b
  
  putStr ("snd=" ++ show c ++ ", mTulp1(8, 'Julie'), isString = ")
  print $ isString c

  putStr ("snd=" ++ show d ++ ", mTulp2('Julie', 8), isString = ")
  print $ isString d

  putStrLn ("tupFunc " ++ show sT1 ++ " " ++ show sT2 ++ " = " ++ show e )


--isChar
--sChar :: Char  
sChar = 'a' :: Char
isChar :: (Typeable a) => a -> Bool
isChar s = typeOf s == typeOf sChar

--isString
--sString :: String  
sString = "test" :: String
isString :: (Typeable a) => a -> Bool
isString s = typeOf s == typeOf sString

--isString2
--sString2 :: [Char]
sString2 = ['t', 'e', 's', 't'] :: [Char]
isString2 :: (Typeable a) => a -> Bool
isString2 s2 = typeOf s2 == typeOf sString2

--isInteger
--nInteger :: Integer  
nInteger = 1 :: Integer
isInteger :: (Typeable a) => a -> Bool
isInteger n = typeOf n == typeOf nInteger

--isInt
--nInt :: Int  
nInt = 1 :: Int
isInt :: (Typeable a) => a -> Bool
isInt n = typeOf n == typeOf nInt

--isFloat
--nFloat :: Float  
nFloat = 1.0 :: Float
isFloat :: (Typeable a) => a -> Bool
isFloat n = typeOf n == typeOf nFloat

--isDouble
--nDouble :: Double  
nDouble = 1.0 :: Double
isDouble :: (Typeable a) => a -> Bool
isDouble n = typeOf n == typeOf nDouble

--isMood
--nMood :: Mood  
dMood = Woot :: Mood
isMood :: (Typeable a) => a -> Bool
isMood n = typeOf n == typeOf dMood


-- fst3, snd3, trd3 for tuples size 3
fst3 :: (a,b,c) -> a 
fst3 (a,b,c) = a
--  
snd3 :: (a,b,c) -> b 
snd3 (a,b,c) = b 
--
trd3 :: (a,b,c) -> c 
trd3 (a,b,c) = c 

--tupFunc    
tupFunc :: (Int, [a])
        -> (Int, [a])
        -> (Int, [a])
tupFunc (a, b) (c, d) = 
  (a + c, b ++ d)  

--changeMood
-- changes Mood only if argument is not type of Mood
-- otherwise it is Mood    
data Mood = Blah | Woot deriving Show
dt1 = Woot -- :: Mood 
dt2 = Blah -- :: Mood
dtX = undefined
--compiles, but does not have an implementaion
--changeMood = undefined :: Mood
changeMood :: Typeable a => a -> Mood
changeMood dtX = 
  if isMood dtX 
    then 
      Woot :: Mood
    else 
      Blah :: Mood

--changeMood2
changeMood2 :: Mood -> Mood
changeMood2 Woot = Blah
changeMood2 Blah = Woot  

{-
-- does not compile
--changeMood3
changeMood3 val
  | (val == Blah) = Woot
  | (val == Woot) = Blah
-}  
-- not the best implementaion
--  Exception if argument is not type of Mood
--changeMood4
changeMood4 :: Mood -> Mood
changeMood4 Blah = Woot
changeMood4    _ = Blah



nInp1 = 5 :: Integer
nInp2 = Woot :: Mood

--showResChMood
showResChMood :: IO () 
showResChMood = do
  let str1 = show $ changeMood nInp1
      str2 = show $ changeMood nInp2    
  putStr str1
  putStrLn " changeMood 5"
  putStr str2
  putStrLn " changeMood Woot"

--addZero
addZero :: String -> String
--addZero (a:[]) = '0' : a : []       -- after hlint, next line
addZero [a] = ['0' , a] 
addZero as     = as

--Numeric types
mI8 = 1 :: Int8
mI16 = 1 :: Int16
mI32 = 1 :: Int32
mI64 = 1 :: Int64
mInt = 1 :: Int
mInteger = 1 :: Integer

mMin8 = minBound :: Int8        -- -128
mMax8 = maxBound :: Int8        --  127
mMin16 = minBound :: Int16      -- -32768
mMax16 = maxBound :: Int16      --  32767
mMin32 = minBound :: Int32      -- -2147483648
mMax32 = maxBound :: Int32      --  2147483647
mMin64 = minBound :: Int64      -- -9223372036854775808
mMax64 = maxBound :: Int64      --  9223372036854775807
mMinInt = minBound :: Int       -- -9223372036854775808
mMaxInt = maxBound :: Int       --  9223372036854775807

--printIntBounds  
printIntBounds :: IO ()
printIntBounds = do
  print mMin8
  print mMax8
  print mMin16 
  print mMax16 
  print mMin32 
  print mMax32 
  print mMin64 
  print mMax64 
  print mMinInt
  print mMaxInt

--printUnicodeStrings
printUnicodeStrings :: IO ()
printUnicodeStrings = do
  putStrLn "abc"                              -- abc
  putStrLn ['^', '$', '&', 'λ', '>', 'a']     -- ^$&λ>a
  putStrLn lambdT                             -- TIO.putStrLn Lambda: λ
  putStrLn "putStrLn Lambda: λ"               -- putStrLn Lambda: λ
  putStrLn aWord1   -- Хорошего нам всем века, года, месяца, дня, жизни! :)
  putStrLn aWord2   -- 哈斯克尔7.6.1
  putStrLn aWord0   -- Cheese
  putStrLn dharma   -- धर्म
  putStrLn bgText   -- श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो
  print aWord1                      -- "\1061\1086\1088\1086\1096\1077\1075\1086 ...
  print aWord2                      -- "\21704\26031\20811\23572\&7.6.1"
  print aWord0                      -- "Cheese"
  print 'a'                         -- 'a'
  print 'λ'                         -- '\955'
  putStrLn "abc"                    -- abc
  putStrLn ['^', '$', '&', 'λ', '>', 'a']   -- ^$&λ>a
  
  putStrLn "===================== Module Lib is Done =================="

aWord0 = "Cheese"
aWord1 = "Хорошего нам всем века, года, месяца, дня, жизни! :)"
aWord2 = "哈斯克尔7.6.1"
dharma = "धर्म"
bgText = " श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो"
lambdT = "TIO.putStrLn Lambda: λ"













